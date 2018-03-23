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
005900030822     D*------------------
006000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006100030822     D*------------------
006200030822     D posDaDft        S              3  0 INZ(*zeros)
006300030822     D posADft         S              3  0 INZ(*zeros)
006400030822     D j               s              3  0 INZ(1)
006500030822     D wGiroDft        s              1  0 INZ(*zeros)
006600080418     D*------------------
006700080418     D* Costanti
006800080418     D*------------------
006900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007100100318
007200100318     D*------------------
007300100318     D* LINKING A DEFINIZIONI ESTERNE
007400100318     D*------------------
007500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007700100318
007800010330
007900010330
008000010330
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400010601     C*
008500010601     C                   exsr      opeini
008600010605     C                   exsr      rwvab
008700010601     C*
008800010601     C                   seton                                        lr
008900010601
009000010601
009100010601
009200010601     C*--------------------------------------------------------
009300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009400010601     C*--------------------------------------------------------
009500010601     C     PREELA        BEGSR
009600010601     C*
009700010601     C* SE OCCORRE SPEDIRE IN FILIALE
009800010601     C                   if        invfil <> *zeros and
009900010601     C                             flgGiro = '0'
010000010601     C*
010100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010200010601     C                   eval      flgGiro = '1'
010300010601     C*
010400010601     C                   endif
010500010601     C*
010600010601     C                   ENDSR
010700010601     C***
010800010601
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600040119     C*
011700040119     C* Effettuo la chiamata al CLLE preposto
011800040119     C                   call(e)   'TITVVTC'
011900040510     C                   parm                    parccm
012000040119     C                   parm                    parmbr
012100040119     C                   parm      '2'           paropz
012200050201     C*
012300050201     C* Effettuo lancio TISI95 solo x chiusura
012400050201     C                   CLEAR                   TISI95DS
012500050201     C                   EVAL      I95TLA = 'C'
012600050201     C                   CALL      'TISI95R'
012700050201     C                   PARM                    TISI95DS
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100000613
013200010601
013300010601
013400010330     C*--------------------------------------------------------
013500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
013600010330     C*--------------------------------------------------------
013700010605     C     RWVAB         BEGSR
013800010330     C*
013900010330     C                   if        not %open(tivin00r)
014000010330     C                   open      tivin00r
014100010330     C                   endif
014200021025     C                   if        not %open(fivabwwr)
014300021025     C                   open      fivabwwr
014400010330     C                   endif
014500040119     C*
014600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014700040119     C                   exsr      prevat
014800040119     C*
014900040119     C                   if        chkcall = '0'
015000040119     C*
015100040119     C                   if        not %open(fivatwwr)
015200040119     C                   open      fivatwwr
015300040119     C                   endif
015400010330     C*
015500040119     C                   clear                   §CTROKVB          7 0
015600040119     C                   clear                   §CTROKVT          7 0
015700040119     C                   clear                   §CTRMO            7 0
015800040119     C                   clear                   §CTRNO            7 0
015900070823     C*
016000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016200100318     C***                exsr      inzvar
016300100318     C***                exsr      impvab
016400010330     C*
016500030822     C                   do        *HIVAL
016600010330     C*
016700030822     C                   read      tivin00r                               70
016800010618     C*
016900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017000010618     C                   if        vinflg = '1'
017100010618     C                   eval      flgOk = '1'
017200010618     C                   endif
017300040510     C*
017400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017500040510     C                   z-add     *zeros        wLenVINDTA        4 0
017600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018000040510     C                   z-add     1             wFlgVINDTA
018100040510     C                   leave
018200040510     C                   endif
018300040510     C                   enddo
018400010618     C*
018500040510     C                   if        vindta > *blanks AND
018600040510     C                             wFlgVINDTA = 1
018700010330     C                   add       1             rrnum
018800010330     C*
018900010601     C                   if        *in70 = *off and
019000010330     C                             (vinflg = *blanks
019100010330     C                              or vinflg = '0'
019200010330     C                              or vinflg = '2')
019300010330     C*
019400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019500010711     C                   if        vinflg = *blanks or vinflg = '0'
019600010711     C                   clear                   vinmsg
019700010711     C                   endif
019800010601     C*
019900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020000070925     C                   eval      vindta  = ' ' + %trim(vindta)
020100070925     C*
020200080527     C                   exsr      inzvar
020300100318     C                   exsr      normCSV
020400010605     C                   exsr      impvab
020500010601     C*
020600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020700010605     C                   eval      depfil = VABLNP
020800010601     C                   exsr      repfil
020900010601     C                   if        depfil = invfil
021000021025     C                   if        vlrpoi = 999
021100030822     C                   move(P)   invfil        VABFGS
021200021025     C                   else
021300030822     C                   move(P)   vlrpoi        VABFGS
021400021025     C                   endif
021500010601     C*
021600010601     C                   exsr      PREELA
021700010601     C*
021800010604     C* Ebbene...
021900010604     C*
022000040119     C  N31              add       1             §CTROKVB
022100040119     C   32              add       1             §CTRMO
022200040119     C   31              add       1             §CTRNO
022300080418     C*
022400030822     C                   if        wGiro = 2
022500080513     C* Scrivo sempre il VAT
022600080513     C  N31              exsr      wrivat
022700080527     C                   exsr      chkvab
022800080513     C                   endif
022900010604     C*
023000010604     C                   if        *in31 = *off and
023100010604     C                             *in32 = *off
023200010604     C                   eval      vinflg = '1'
023300010604     C                   else
023400010604     C                   eval      vinflg = '2'
023500010604     C                   endif
023600010604     C                   endif
023700010601     C*
023800010604     C                   endif
023900010604     C*
024000010330     C                   else
024100010330     C                   eval      vinflg = '1'
024200010330     C                   endif
024300010601     C*
024400010601     C  N70              update    tivin000
024500010330     C*
024600030822     C  N70              enddo
024700040119     C*
024800080527     C* Verifico dati rimasti "in cannna"...
024900080527     C                   seton                                        65
025000080527     C                   exsr      chkvab
025100080527     C*
025200040119     C                   endif                                                  (endif - chkcall)
025300010601     C*
025400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025500010601     C                   if        cntNonEl = *zeros or
025600010601     C                             flgMulti = '0'
025700010330     C* Se non ci sono record con errori ...
025800010601     C                   if        §ctrno = 0 and
025900010604     C                             §ctrmo = 0 and
026000010601     C                             flgStato <> '2'
026100010330     C* ... restituisco esito OK.
026200010330     C                   eval      wrkesito = '0'
026300010330     C                   else
026400040119     C                   if        §ctrokvb > 0
026500010330     C                   eval      wrkesito = '1'
026600010330     C                   else
026700010615     C                   if        flgOk = '0'
026800010615     C                   eval      wrkesito = '2'
026900010615     C                   else
027000010615     C                   eval      wrkesito = '6'
027100010615     C                   endif
027200010330     C                   endif
027300010330     C                   endif
027400010601     C                   else
027500010601     C                   eval      wrkesito = '9'
027600010601     C                   endif
027700010330     C*
027800010330     C                   if        %open(tivin00r)
027900010330     C                   close     tivin00r
028000010330     C                   endif
028100021025     C                   if        %open(fivabwwr)
028200021025     C                   close     fivabwwr
028300010330     C                   endif
028400040512     C                   if        %open(fivatwwr)
028500040512     C                   close     fivatwwr
028600040512     C                   endif
028700010601     C*
028800010601     C                   if        vlrpoi <> 999
028900010601     C                   eval      invfil = vlrpoi
029000010601     C                   endif
029100010330     C*
029200040119     C                   if        §ctrokvb > 0
029300010601     C                             and invfil > *zeros
029400010330     C                   exsr      invio
029500010330     C                   endif
029600010601     C*
029700010618     C                   if        flgGiro = '1'
029800010601     C                   exsr      endela
029900010618     C                   endif
030000010330     C*
030100010330     C                   ENDSR
030200010330     C***
030300080527
030400080527
030500080527
030600080527     C*----------------------------------------------------*
030700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
030800080527     C*----------------------------------------------------*
030900100413     C     CHKVAB        BEGSR
031000080527     C*
031100080527     C* Scrivo il VAB solo a rottura d spedizione
031200080527     C                   if        depspe = *zeros
031300080527     C  N31              eval      fivabds_sav = fivabds
031400080527     C                   eval      depspe = vabNSP
031500080527     C                   else
031600080527     C                   if        depspe = vabNSP AND *in65 = *off
031700100413     C   51              add       VABNCL        sav_VABNCL
031800100413     C   51              add       VABPKB        sav_VABPKB
031900100413     C   51              add       VABVLB        sav_VABVLB
032000100413     C  N51              z-add     VABNCL        sav_VABNCL
032100100413     C  N51              z-add     VABPKB        sav_VABPKB
032200100413     C  N51              z-add     VABVLB        sav_VABVLB
032300080527     C                   else
032400080527     C  N31              eval      fivabds_ok = fivabds_sav
032500080527     C  N31              exsr      wrivab
032600080527     C  N31              eval      fivabds_sav = fivabds
032700080527     C                   eval      depspe = vabNSP
032800080527     C                   endif
032900080527     C                   endif
033000080527     C*
033100080527     C                   ENDSR
033200040119
033300040119
033400040119
033500040119     C*----------------------------------------------------*
033600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
033700040119     C*----------------------------------------------------*
033800040510     C     WRIVAB        BEGSR
033900080513     C*
034000080513     C* Gestisco l'eventuale rottura x numero spedizione
034100080527     C                   if        VABNSP <> depspe or *in65 = *on
034200070301     C                   if        wVATNOT_E <> *blanks
034300070301     C                   eval      VABCTM = '7Q'
034400070301     C                   endif
034500100413     C                   write     fivab000
034600080513     C                   endif
034700040119     C*
034800040119     C                   ENDSR
034900040119
035000040119
035100040119
035200040119     C*----------------------------------------------------*
035300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
035400040119     C*----------------------------------------------------*
035500040510     C     WRIVAT        BEGSR
035600060512     C*
035700060512     C* Inizializzo i campi chiave
035800060512     C                   eval      VATFGS = VABFGS
035900060512     C                   eval      VATAAS = VABAAS
036000060512     C                   eval      VATCCM = VABCCM
036100070823     C                   eval      VATNRS = VABNRS
036200060512     C                   eval      VATNSP = VABNSP
036300060512     C                   eval      VATLNP = VABLNP
036400040714     C*
036500040714     C* Solo se VATNOT valorizzato scarico il buffer
036600040714     C                   if        VATNOT <> *blanks
036700040119     C*
036800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
036900040510     C                   if        VATTRC = *blanks
037000040510     C                   eval      VATTRC = 'E'
037100040510     C                   endif
037200040510     C                   write     FIVAT000
037300040119     C                   add       1             §CTROKVT
037400040714     C*
037500040714     C                   endif
037600060331     C*
037700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
037800060331     C                   if        wVATNOT_A <> *blanks
037900060331     C                   eval      VATNOT = wVATNOT_A
038000060331     C                   eval      VATTRC = 'A'
038100060331     C                   write     FIVAT000
038200060331     C                   add       1             §CTROKVT
038300060331     C                   endif
038400060331     C*
038500060331     C                   if        wVATNOT_B <> *blanks
038600060331     C                   eval      VATNOT = wVATNOT_B
038700060331     C                   eval      VATTRC = 'B'
038800060331     C                   write     FIVAT000
038900060331     C                   add       1             §CTROKVT
039000060331     C                   endif
039100060331     C*
039200060331     C                   if        wVATNOT_E <> *blanks
039300060331     C                   eval      VATNOT = wVATNOT_E
039400060331     C                   eval      VATTRC = 'E'
039500060331     C                   write     FIVAT000
039600060331     C                   add       1             §CTROKVT
039700060331     C                   endif
039800040119     C*
039900040119     C                   ENDSR
040000040119
040100040119
040200040119
040300040119     C*----------------------------------------------------*
040400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040500040119     C*----------------------------------------------------*
040600040119     C     PREVAT        BEGSR
040700040119     C*
040800040119     C* Compongo il nome del membro da dare al FIVATWWR
040900040119     C                   eval      parmbr = vlrhdl
041000040119     C                   movel     'M'           parmbr
041100040510     C                   eval      parccm = vlrksc
041200040119     C                   eval      paropz = '1'
041300040119     C* Effettuo la chiamata al CLLE preposto
041400040119     C                   call(e)   'TITVVTC'
041500040119     C                   parm                    parccm
041600040119     C                   parm                    parmbr
041700040119     C                   parm                    paropz
041800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041900040119     C                   if        %error
042000040119     C                   movel     '1'           chkcall
042100040119     C                   else
042200040119     C                   movel     '0'           chkcall
042300040119     C                   endif
042400040119     C*
042500040119     C                   ENDSR
042600010601
042700010601
042800010601
042900010330     C*----------------------------------------------------*
043000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
043100010330     C*----------------------------------------------------*
043200040119     C     INZVAR        BEGSR
043300080527     C*
043400080527     C                   setoff                                       65
043500010330     C*
043600030822     C* Inizializzo variabili di wrk
043700020204     C                   Z-ADD     *zeros        Num5_0            5 0
043800060331     C                   MOVEL     *blanks       wVATNOT_A
043900060331     C                   MOVEL     *blanks       wVATNOT_B
044000060331     C                   MOVEL     *blanks       wVATNOT_E
044100100824     C                   MOVEL     *blanks       wNOTE            80
044200030822     C*
044300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
044400030822     C                   CLEAR                   SkSplitCSV
044500030822     C                   EVAL      i = 1
044600030822     C                   EVAL      posDa = *zeros
044700030822     C                   EVAL      posA  = *zeros
044800080527     C*
044900080527     C                   CLEAR                   FIVABDS
045000080527     C                   CLEAR                   FIVAT000
045100030822     C*
045200030822     C* Reimposto i valori di default
045300030822     C                   EXSR      DEFCAM
045400030822     C*
045500010330     C                   ENDSR
045600010330     C*----------------------------------------------------*
045700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
045800010330     C*----------------------------------------------------*
045900010330     C     DEFCAM        BEGSR
046000030822     C*
046100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046200030715     C* e delimitatore testo.
046300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046600100318     C*
046700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
046800100318     C                   EVAL      CharSOS = CharNUM
046900030822     C*
047000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
047100100413     C                   SETOFF                                       5051
047200100413     C                   SELECT
047300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
047400030715     C                   SETON                                        50
047500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
047600100413     C                   SETON                                        51
047700100413     C                   ENDSL
047800030822     C*
047900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
048000030822     C                   EVAL      posDaDft = 1
048100030822     C                   EVAL      posADft  = 0
048200030822     C                   EVAL      wGiroDft = 0
048300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
048400030822     C                             posDaDft > 0
048500030822     C*
048600030822     C* Gestisco il 1° giro
048700030822     C                   IF        wGiroDft = 0
048800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
048900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
049000030822     C* Incremento il contatore dei "giri"
049100030822     C                   EVAL      wGiroDft = 1
049200030822     C                   ELSE
049300030822     C                   EVAL      posDaDft = posADft
049400030822     C                   ENDIF
049500030822     C* Eseguo lo scan x trovare la fine del campo corrente
049600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
049700030822     C*
049800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
049900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
050000030822     C                   IF        posDaDft > 0 AND
050100030822     C                             posADft  > 0
050200030822     C* NCL
050300030822     C                   IF        %subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):1:3)
050600030822     C                             = 'NCL'
050700030822     C                   EVAL      PiStr=%trim(%subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051000030822     C                   EXSR      CHKNUM
051100030822     C                   IF        PiInt=*on
051200030822     C                   Z-ADD     PiVal         VABNCL
051300030822     C                   ENDIF
051400030822     C                   ENDIF
051500030822     C* CCM
051600030822     C                   IF        %subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):1:3)
051900030822     C                             = 'CCM'
052000030822     C                   EVAL      PiStr=%trim(%subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052300030822     C                   EXSR      CHKNUM
052400030822     C                   IF        PiInt=*on
052500030822     C                   Z-ADD     PiVal         VABCCM
052600030822     C                   ENDIF
052700030822     C                   ENDIF
052800030822     C* LNP
052900030822     C                   IF        %subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):1:3)
053200030822     C                             = 'LNP'
053300030822     C                   EVAL      PiStr=%trim(%subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053600030822     C                   EXSR      CHKNUM
053700030822     C                   IF        PiInt=*on
053800030822     C                   Z-ADD     PiVal         VABLNP
053900030822     C                   ENDIF
054000030822     C                   ENDIF
054100030822     C* NRS
054200030822     C                   IF        %subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):1:3)
054500030822     C                             = 'NRS'
054600030822     C                   EVAL      PiStr=%trim(%subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054900030822     C                   EXSR      CHKNUM
055000030822     C                   IF        PiInt=*on
055100030822     C                   Z-ADD     PiVal         VABNRS
055200030822     C                   ENDIF
055300030822     C                   ENDIF
055400030822     C* CTR
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'CTR'
055900030822     C                   EVAL      PiStr=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   EXSR      CHKNUM
056300030822     C                   IF        PiInt=*on
056400030822     C                   Z-ADD     PiVal         VABCTR
056500030822     C                   ENDIF
056600030822     C                   ENDIF
056700030822     C* PKB
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'PKB'
057200030822     C                   EVAL      PiStr=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   EXSR      CHKNUM
057600030822     C                   IF        PiNum=*on
057700030822     C                   Z-ADD     PiVal         VABPKB
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* VLB
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'VLB'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiNum=*on
059000030822     C                   Z-ADD     PiVal         VABVLB
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* QFT
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'QFT'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiNum=*on
060300030822     C                   Z-ADD     PiVal         VABQFT
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* CBO
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'CBO'
061100030822     C                   EVAL      VABCBO=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   ENDIF
061500030822     C* TSP
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'TSP'
062000030822     C                   EVAL      VABTSP=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   ENDIF
062400030822     C* VAS
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'VAS'
062900030822     C                   EVAL      VABVAS=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   ENDIF
063300030822     C* VCA
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'VCA'
063800030822     C                   EVAL      VABVCA=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200070925     C* TIC
064300070925     C                   IF        %subst(
064400070925     C                             %subst(vlrppt:posDaDft+1:
064500070925     C                             posADft-posDaDft-1):1:3)
064600070925     C                             = 'TIC'
064700070925     C                   EVAL      VABTIC=%trim(%subst(
064800070925     C                             %subst(vlrppt:posDaDft+1:
064900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000070925     C                   ENDIF
065100030822     C* GCA
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'GCA'
065600030822     C                   EVAL      VABGCA=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* CTM
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'CTM'
066500030822     C                   EVAL      VABCTM=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* FFD
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'FFD'
067400030822     C                   EVAL      VABFFD=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* VAD
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'VAD'
068300030822     C                   EVAL      VABVAD=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* GMA
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'GMA'
069200030822     C                   EVAL      VABGMA=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* GGA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'GGA'
070100030822     C                   EVAL      VABGGA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* GVA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'GVA'
071000030822     C                   EVAL      VABGVA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* TC1
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'TC1'
071900030822     C                   EVAL      VABTC1=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* TC2
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'TC2'
072800030822     C                   EVAL      VABTC2=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200040714     C* VATTRC
073300040714     C                   IF        %subst(
073400040714     C                             %subst(vlrppt:posDaDft+1:
073500040714     C                             posADft-posDaDft-1):1:3)
073600040714     C                             = 'TRC'
073700040714     C                   EVAL      VATTRC=%trim(%subst(
073800040714     C                             %subst(vlrppt:posDaDft+1:
073900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000040714     C                   ENDIF
074100030822     C* ...
074200030822     C                   ENDIF
074300030822     C                   ENDDO
074400020204     C*
074500010330     C                   ENDSR
074600010607     C*----------------------------------------------------*
074700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
074800010607     C*----------------------------------------------------*
074900010607     C     IMPVAB        BEGSR
075000010607     C*
075100010607     C                   SETOFF                                       3132
075200030515     C*
075300010607     C                   MOVEL     datcor        VABAAS
075400010607     C                   MOVE      datcor        VABMGS
075500010607     C*
075600030715     C**********
075700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
075800030715     C**********
075900030715     C                   IF        wGiro = *zeros
076000040113     C*
076100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
076200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
076300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
076400080429     C                             %trim(intcol(5))<>*blanks
076500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
076600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
076700080429     C                                      %trim(intcol(5))
076800070823     C     ';':CharCSV   XLATE     vindta        vindta
076900040510     C                   ENDIF
077000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077100031201     C     minu:maiu     XLATE     vindta        vindta
077200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077300030715     C                   DOW       posDa <= %len(%trim(vindta))
077400030715     C*
077500030715     C* Gestisco il 1° campo
077600030715     C                   IF        i = 1
077700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077800030715     C                   EVAL      posDa = 1
077900030715     C* Eseguo lo scan x trovare la fine del primo campo
078000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078100030715     C* A questo punto "estraggo" il campo corrente
078200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078300030715     C                                                    (posA-posDa))
078400030715     C* X i campi successivi al 1°
078500030715     C                   ELSE
078600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078700030715     C                   EVAL      posDa = posA + 1
078800030715     C* Eseguo lo scan x trovare la fine del campo corrente
078900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079000030715     C* Gestisco l'ultimo campo
079100030715     C                   IF        posA = *zeros
079200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
079300030715     C* A questo punto "estraggo" il campo corrente
079400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079500030715     C                                                    (posA-posDa))
079600030715     C                   LEAVE
079700030715     C                   ELSE
079800030715     C* A questo punto "estraggo" il campo corrente
079900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080000030715     C                                                    (posA-posDa))
080100030715     C                   ENDIF
080200030715     C                   ENDIF
080300030715     C* Incremento il contatore d campo
080400030715     C                   EVAL      i = i +1
080500030715     C                   ENDDO
080600030715     C*
080700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080800030715     C* delimitazione testo
080900030715     C                   EVAL      i = 1
081000030715     C                   DOW       i < %elem(SkSplitFLD)
081100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081300030715     C                   EVAL      i = i + 1
081400030715     C                   ENDDO
081500030715     C                   EVAL      wGiro = 1
081600030715     C                   ELSE
081700030715     C                   EVAL      wGiro = 2
081800031204     C**********
081900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082000031204     C* d separatore campo
082100031204     C**********
082200031204     C                   EVAL      vindta = %trim(vindta)
082300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
082400031204     C                   EVAL      vindta = %subst(vindta:2)
082500031204     C                   ENDDO
082600031204     C*
082700031204     C                   Z-ADD     *zeros        lunghInput        4 0
082800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
082900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083200031204     C                   ENDDO
083300030715     C**********
083400030715     C* Effettuo lo split del campo dati d input
083500030715     C**********
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
084600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084700030715     C                                                    (posA-posDa))
084800030715     C* X i campi successivi al 1°
084900030715     C                   ELSE
085000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085100030715     C                   EVAL      posDa = posA + 1
085200031204     C* Verifico che nn vi sia il campo nn valorizzato
085300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
085400031204     C* Se campo nn valorizzato skippo al prossimo
085500031204     C                   EVAL      SkSplitCSV(i) = *blanks
085600031204     C                   EVAL      posA  = posA + 1
085700031204     C                   ELSE
085800030715     C* Eseguo lo scan x trovare la fine del campo corrente
085900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086000030715     C* Gestisco l'ultimo campo
086100030715     C                   IF        posA = *zeros
086200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086300030715     C* A questo punto "estraggo" il campo corrente
086400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086500030715     C                                                    (posA-posDa))
086600030715     C                   LEAVE
086700030715     C                   ELSE
086800030715     C* A questo punto "estraggo" il campo corrente
086900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087000030715     C                                                    (posA-posDa))
087100030715     C                   ENDIF
087200031204     C                   ENDIF
087300030715     C                   ENDIF
087400030715     C* Incremento il contatore d campo
087500030715     C                   EVAL      i = i +1
087600030715     C                   ENDDO
087700030715     C*
087800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087900030715     C* delimitazione testo
088000030715     C                   EVAL      i = 1
088100030715     C                   DOW       i < %elem(SkSplitCSV)
088200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
088300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
088400030715     C                   EVAL      i = i + 1
088500030715     C                   ENDDO
088600100720     C*
088700100720     C* Stacco un progressivo x numero spedizione
088800100720 xxx C***                EXSR      repNSP
088900100326     C*
089000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089100030715     C                   EVAL      i = 1
089200030715     C                   DOW       i < %elem(SkSplitFLD)
089300030715     C*
089400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
089600030715     C                             SkSplitCSV(i) <> *blanks
089700030721     C*** ATB
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
089900030721     C                   EVAL      VABATB = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030715     C*** RSD
090200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
090600030715     C* ==
090700030715     C                   ENDIF
090800030721     C*** RD2
090900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091100030721     C                   ENDIF
091200030715     C*** IND
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091400030715     C                   EVAL      VABIND = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030715     C*** LOD
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** PRD
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030721     C*** NZD
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030715     C*** RMA
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093100030715     C                   ENDIF
093200030715     C*** NOT
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030715     C*** NT2
093700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
093900030715     C                   ENDIF
094000030715     C*** VCA
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094300030715     C                   ENDIF
094400030715     C*** VAS
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094700030715     C                   ENDIF
094800030715     C*** FFD
094900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095100030715     C                   ENDIF
095200030721     C*** GC1
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** GC2
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
095800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** TSP
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096300030721     C                   ENDIF
096400040510     C*** CBO
096500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
096600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
096700040510     C                   ENDIF
096800030721     C*** NAS
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** TIC
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** GCA
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
097800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** XCO
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** CTM
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
098600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** TCR
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** CTS
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** FTM
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
099800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** VAD
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GMA
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
100600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** GGA
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** GVA
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** TC1
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
101800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TC2
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** SCL
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
102600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** RMO
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** NMO
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*
103700030721     C* Reperisco quindi i campi numerici...
103800040510     C*** NSP
103900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104000040510     C                   EVAL      PiStr=SkSplitCSV(i)
104100040510     C                   EXSR      CHKNUM
104200040510     C                   IF        PiInt=*on
104300040510     C                   Z-ADD     PiVal         VABNSP
104400040510     C                   ELSE
104500040510     C                   SETON                                        31
104600040510     C                   EVAL      VABNSP = *zeros
104700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
104800040510     C                             + ' ' + 'VABNSP'
104900040510     C                   ENDIF
105000040510     C                   ENDIF
105100071106     C*** RMN
105200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105300071106     C                   EVAL      PiStr=SkSplitCSV(i)
105400071106     C                   EXSR      CHKNUM
105500071106     C                   IF        PiInt=*on
105600071106     C                   Z-ADD     PiVal         VABRMN
105700071106     C                   ELSE
105800071106     C                   SETON                                        32
105900071106     C                   EVAL      VABRMN = *zeros
106000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
106100071106     C                             + ' ' + 'VABRMN'
106200071106     C                   ENDIF
106300071106     C                   ENDIF
106400030721     C*** CAD
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106600030721     C                   EVAL      PiStr=SkSplitCSV(i)
106700030721     C                   EXSR      CHKNUM
106800030721     C                   IF        PiInt=*on
106900030721     C                   Z-ADD     PiVal         Num5_0
107000030721     C                   MOVEL(p)  Num5_0        VABCAD
107100030721     C                   ELSE
107200030721     C                   SETON                                        32
107300030721     C                   EVAL      VABCAD = *zeros
107400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107500030721     C                             + ' ' + 'VABCAD'
107600030721     C                   ENDIF
107700030721     C                   ENDIF
107800030721     C*** NCL
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108000030721     C                   EVAL      PiStr=SkSplitCSV(i)
108100030721     C                   EXSR      CHKNUM
108200030721     C                   IF        PiInt=*on
108300030721     C                   Z-ADD     PiVal         VABNCL
108400030721     C                   ELSE
108500030721     C                   SETON                                        32
108600030721     C                   EVAL      VABNCL = *zeros
108700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108800040122     C                             + ' ' + 'VABNCL'
108900030721     C                   ENDIF
109000030721     C                   ENDIF
109100030721     C*** PKB
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109300030721     C                   EVAL      PiStr=SkSplitCSV(i)
109400030721     C                   EXSR      CHKNUM
109500030721     C                   IF        PiNum=*on
109600030721     C                   Z-ADD     PiVal         VABPKB
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      VABPKB = *zeros
110000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110100030721     C                             + ' ' + 'VABPKB'
110200030721     C                   ENDIF
110300030721     C                   ENDIF
110400030721     C*** CAS
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110600030721     C                   EVAL      PiStr=SkSplitCSV(i)
110700030721     C                   EXSR      CHKNUM
110800030721     C                   IF        PiNum=*on
110900030721     C                   Z-ADD     PiVal         VABCAS
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      VABCAS = *zeros
111300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111400030721     C                             + ' ' + 'VABCAS'
111500030721     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** CCM
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on      AND
112200030721     C                             PiVal<=9999999 AND
112300030721     C                             PiVal>*zeros
112400030721     C                   Z-ADD     PiVal         VABCCM
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700030721     C                   EVAL      VABCCM = *zeros
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900040510     C                             + ' ' + 'VABCCM'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** LNP
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on    AND
113700030721     C                             PiVal<=999   AND
113800030721     C                             PiVal>*zeros
113900030721     C                   Z-ADD     PiVal         VABLNP
114000030721     C                   ELSE
114100040715     C                   SETON                                        32
114200030721     C                   EVAL      VABLNP = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400040510     C                             + ' ' + 'VABLNP'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** NRS
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on AND
115200030721     C                             PiVal<=99
115300030721     C                   Z-ADD     PiVal         VABNRS
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABNRS'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** LNA
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on    AND
116500030721     C                             PiVal<=999   AND
116600030721     C                             PiVal>*zeros
116700030721     C                   Z-ADD     PiVal         VABLNA
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABLNA = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABLNA'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** CTR
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiInt=*on  AND
118000030721     C                             PiVal<=999
118100030721     C                   Z-ADD     PiVal         VABCTR
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500030721     C                             + ' ' + 'VABCTR'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800030721     C*** IAS
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiNum=*on
119300030721     C                   Z-ADD     PiVal         VABIAS
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABIAS'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** VLB
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiNum=*on
120500030721     C                   Z-ADD     PiVal         VABVLB
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABVLB'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** QFT
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiNum=*on
121700030721     C                   Z-ADD     PiVal         VABQFT
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABQFT'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** NCD
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on      AND
122900030721     C                             PiVal<=9999999
123000030721     C                   Z-ADD     PiVal         VABNCD
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABNCD'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** NCA
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on      AND
124200030721     C                             PiVal<=9999999
124300030721     C                   Z-ADD     PiVal         VABNCA
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABNCA'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** ZNC
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on AND
125500030721     C                             PiVal<=99
125600030721     C                   Z-ADD     PiVal         VABZNC
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABZNC'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** DCR
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on       AND
126800030721     C                             PiVal<=99999999
126900030721     C                   Z-ADD     PiVal         VABDCR
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABDCR'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** HCR
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on   AND
128100030721     C                             PiVal<=9999
128200030721     C                   Z-ADD     PiVal         VABHCR
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABHCR'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** VMD
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiNum=*on
129400030721     C                   Z-ADD     PiVal         VABVMD
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABVMD'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** ANT
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on        AND
130600030721     C                             PiVal<=999999999
130700030721     C                   Z-ADD     PiVal         VABANT
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABANT'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** CMO
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on
131900030721     C                   Z-ADD     PiVal         Num5_0
132000030721     C                   MOVEL(p)  Num5_0        VABCMO
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      VABCMO = *zeros
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABCMO'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800040510     C*
132900040510     C* Reperisco i campi del FIVAT
133000040510     C*** VATNOT
133100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133300040510     C                   ENDIF
133400040510     C*** VATTRC
133500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133700040510     C                   ENDIF
133800060331     C*** VATNOT_A
133900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
134000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134100060331     C                   ENDIF
134200060331     C*** VATNOT_B
134300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134500060331     C                   ENDIF
134600060331     C*** VATNOT_E
134700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
134800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
134900060331     C                   ENDIF
135000070823     C*
135100071029     C***  ===>  Gestione campi "particolari" dentro ciclo
135200100720     C*** RMA / NSP
135300100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
135400100419     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
135500100413     C                   EVAL      PiStr=SkSplitCSV(i)
135600100413     C                   EXSR      CHKNUM
135700100413     C                   IF        PiInt=*on
135800100419     C                   Z-ADD     PiVal         VABNSP
135900100413     C                   ELSE
136000100413     C                   SETON                                        32
136100100413     C                   EVAL      vinmsg = %trimr(vinmsg)
136200100720     C                             + ' ' + 'VABNSP'
136300100413     C                   ENDIF
136400100326     C                   ENDIF
136500100720     C*** VLB
136600100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
136700100720     C                   EVAL      PiStr=SkSplitCSV(i)
136800100720     C                   EXSR      CHKNUM
136900100720     C                   IF        PiNum=*on
137000100720     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
137100100720     C                   Z-ADD     PiVal         VABVLB
137200100720     C                   ELSE
137300100720     C                   SETON                                        32
137400100720     C                   EVAL      vinmsg = %trimr(vinmsg)
137500100720     C                             + ' ' + 'VABVLB'
137600100720     C                   ENDIF
137700100720     C                   ENDIF
137800100419     C*** CBO
137900100419     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
138000100419     C                   SELECT
138100100720     C                   WHEN      %trim(SkSplitCSV(i)) = 'X'
138200100720     C                   EVAL      VABCBO = '4'
138300100419     C                   ENDSL
138400100413     C                   ENDIF
138500100720     C*** NOT / NT2
138600100824     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_1'
138700100824     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
138800100824     C                                     SkSplitCSV(i)
138900100720     C                   ENDIF
139000100824     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_2'
139100100824     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
139200100824     C                                     SkSplitCSV(i)
139300100824     C                   ENDIF
139400070925     C*
139500071029     C***  <===  -----------------------------------------
139600040510     C*
139700030715     C                   ENDIF
139800030715     C                   EVAL      i = i + 1
139900030715     C                   ENDDO
140000070925     C*
140100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
140200100824     C*** NOT / NT2
140300100824     C                   EVAL      wNOTE = %trim(wNOTE)
140400100824     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
140500100824     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
140600070925     C*
140700071029     C***  <===  -----------------------------------------
140800070925     C*
140900070925     C* Considerazioni finali su CBO/CAS
141000100318     C                   IF        wFlgCAS = '1'    OR
141100100318     C                             VABCAS  > *zeros
141200070925     C                   IF        VABCBO = '1'
141300070925     C                   EVAL      VABCBO = '4'
141400070925     C                   ENDIF
141500070925     C                   IF        VABCBO = '2'
141600070925     C                   EVAL      VABCBO = '6'
141700070925     C                   ENDIF
141800070925     C                   ENDIF
141900070925     C***  <===  ----------------------------
142000040510     C*
142100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
142200040510     C                   IF        VABNSP = *zeros
142300040510     C                   SETON                                        31
142400040510     C                   ENDIF
142500040715     C*
142600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
142700040715     C                   IF        VABLNP = *zeros
142800040715     C                   SETON                                        31
142900040715     C                   ENDIF
143000040510     C*
143100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
143200040510     C                   IF        VABRMN = *zeros
143300040510     C                   Z-ADD     VABNSP        VABRMN
143400040510     C                   ENDIF
143500040510     C*
143600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
143700040510     C                   IF        VABRMA = *blanks
143800040714     C                   MOVEL     VABRMN        VABRMA
143900040510     C                   ENDIF
144000040128     C*
144100040510     C* Se provincia nn valorizzata la reperisco
144200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
144300040510     C                   IF        VABPRD = *blanks
144400040128     C                   CLEAR                   TISI95DS
144500040128     C                   EVAL      I95TCN = '3'
144600040128     C                   Z-ADD     datcor        I95DAT
144700040128     C                   EVAL      I95NAR = VABNZD
144800040128     C                   EVAL      I95CAP = VABCAD
144900040128     C                   EVAL      I95LOC = VABLOD
145000040128     C                   CALL      'TISI95R'
145100040128     C                   PARM                    TISI95DS
145200040128     C                   EVAL      VABPRD = O95PRV
145300040510     C                   ENDIF
145400020204     C*
145500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
145600020204     C                   EXSR      CHKIMPDIV
145700030715     C                   ENDIF
145800020204     C*
145900010607     C                   ENDSR
146000010607     C*----------------------------------------------------*
146100010601
146200020204
146300020204     C*----------------------------------------------------*
146400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
146500020204     C*----------------------------------------------------*
146600020204     C     CHKIMPDIV     BEGSR
146700020204     C*
146800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
146900020204     C                   Z-ADD     *zeros        wrkDec            9 9
147000020204     C*
147100020204     C* Come prima cosa effettuo considerazioni sulla divisa
147200020204     C                   IF        vabIAS > *zeros
147300020204     C                   IF        vabVAS <> 'EUR'
147400020204     C                   EVAL      vabVAS =  'ITL'
147500020204     C                   ENDIF
147600020204     C                   ENDIF
147700020204     C*
147800020204     C                   IF        vabCAS > *zeros
147900020204     C                   IF        vabVCA <> 'EUR'
148000020204     C                   EVAL      vabVCA =  'ITL'
148100020204     C                   ENDIF
148200020204     C                   ENDIF
148300020204     C*
148400020204     C                   IF        vabVMD > *zeros
148500020204     C                   IF        vabVAD <> 'EUR'
148600020204     C                   EVAL      vabVAD =  'ITL'
148700020204     C                   ENDIF
148800020204     C                   ENDIF
148900020204     C*
149000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
149100020204     C                   Z-ADD     vabIAS        wrkDec
149200020204     C                   IF        wrkDec > *zeros
149300020204     C                   IF        vabVAS = 'ITL'
149400020204     C                   EVAL      vabIAS = *zeros
149500020204     C                   ENDIF
149600020204     C                   ENDIF
149700020204     C*
149800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
149900020204     C                   Z-ADD     vabCAS        wrkDec
150000020204     C                   IF        wrkDec > *zeros
150100020204     C                   IF        vabVCA = 'ITL'
150200020204     C                   EVAL      vabCAS = *zeros
150300020204     C                   ENDIF
150400020204     C                   ENDIF
150500020204     C*
150600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
150700020204     C                   Z-ADD     vabVMD        wrkDec
150800020204     C                   IF        wrkDec > *zeros
150900020204     C                   IF        vabVAD = 'ITL'
151000020204     C                   EVAL      vabVMD = *zeros
151100020204     C                   ENDIF
151200020204     C                   ENDIF
151300020204     C*
151400020204     C                   ENDSR
151500020204     C***
151600010330
151700100318
151800100318
151900100318     C*----------------------------------------------------*
152000100318     C*  CONTROLLO NUMERICITA' CAMPI
152100100318     C*----------------------------------------------------*
152200100318     C     CHKNUM        BEGSR
152300100318     C*
152400100318     C                   IF        PiDecChr = *blanks
152500100318     C                   EVAL      PiDecChr = CharNUM
152600100318     C                   ENDIF
152700100318     C*
152800100318     C                   callp     UBISNUM_Check(PiStr
152900100318     C                                          :PiDecChr
153000100318     C                                          :PiVal
153100100318     C                                          :PiNum
153200100318     C                                          :PiInt)
153300100318     C*
153400100318     C                   ENDSR
153500100318     C***
153600010601
153700010601
153800010601
153900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
154000010601     C     repfil        BEGSR
154100010601     C*
154200010601     C                   if        invfil = *zeros and
154300010601     C                             depfil > *zeros and
154400010629     C                             (vinflg = *blanks or
154500010629     C                              vinflg = *zeros)
154600010601     C
154700010601     C                   eval      invfil = depfil
154800010601     C                   endif
154900010601     C*
155000010601     C                   if        depfil <> invfil and
155100010601     C                             invfil > *zeros
155200010601     C                   eval      flgMulti = '1'
155300010601     C                   if        vinflg = *blanks
155400010601     C                   add       1             cntNonEl
155500010601     C                   endif
155600010601     C                   endif
155700010601     C*
155800010601     C                   if        vinflg = '2'
155900010601     C                   eval      flgStato = '2'
156000010601     C                   endif
156100010601     C*
156200010601     C                   ENDSR
156300010601     C***
156400010601
156500010601
156600010601
156700010330
156800040119      /TITLE Invio dei dati al punto operativo.
156900040119     C     invio         BEGSR
157000040119     C*
157100040119     C* 1° invio FIVAT
157200040119     C                   reset                   dscmz
157300040119     C                   move      vlrpoi        cmzdst
157400040119     C                   eval      cmzfld = 'FIVATWWR'
157500040119     C                   eval      cmzmbd = vlrhdl
157600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
157700040119     C***                if        prmfir = *blanks
157800040119     C                   eval      cmzfla = 'FIVAT00F'
157900040119     C                   eval      cmzmba = 'FIVAT00F'
158000040119     C***                else
158100040119     C***                eval      cmzfla = prmfir
158200040119     C***                eval      cmzmba = prmfir
158300040119     C***                endif
158400040119     C                   eval      cmznrr = *zeros
158500040119     C                   move      §ctrokvt      cmznrr
158600040119     C                   eval      cmzlba = vlrfl1
158700040119     C                   call(e)   'TIS711C'
158800040119     C                   parm                    dscmz
158900040119     C                   parm      *blanks       esito
159000040119     C                   if        %error
159100040119     C                             or cmzerr = '1'
159200040119     C                             or esito  = '1'
159300040119     C                   eval      wrkesito = '3'
159400040119     C                   else
159500040119     C*
159600040119     C* 2° invio FIVAB
159700040119     C                   reset                   dscmz
159800040119     C                   move      vlrpoi        cmzdst
159900040119     C                   eval      cmzfld = vlrfou
160000040119     C                   eval      cmzmbd = vlrhdl
160100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160200040119     C***                if        prmfir = *blanks
160300040119     C                   eval      cmzfla = 'FIVAB00F'
160400040119     C                   eval      cmzmba = 'FIVAB00F'
160500040119     C***                else
160600040119     C***                eval      cmzfla = prmfir
160700040119     C***                eval      cmzmba = prmfir
160800040119     C***                endif
160900040119     C                   eval      cmznrr = *zeros
161000040119     C                   move      §ctrokvb      cmznrr
161100040119     C                   eval      cmzlba = vlrfl1
161200040119     C                   call(e)   'TIS711C'
161300040119     C                   parm                    dscmz
161400040119     C                   parm      *blanks       esito
161500040119     C                   if        %error
161600040119     C                             or cmzerr = '1'
161700040119     C                             or esito  = '1'
161800040119     C                   eval      wrkesito = '3'
161900040119     C                   endif
162000040119     C                   endif
162100040119     C*
162200040119     C                   ENDSR
162300040119     C***
162400010601
162500010601
162600010601
162700010601
162800010601      /TITLE Invio dei dati al punto operativo.
162900010601     C     opeini        BEGSR
163000010601     C*
163100010601     C* Inizializzo flag e contatori operativi
163200010601     C                   movel     '0'           flgGiro           1
163300010601     C                   movel     '0'           flgMulti          1
163400010601     C                   movel     '1'           flgStato          1
163500010615     C                   movel     '0'           flgOk             1
163600010601     C                   z-add     *zeros        cntNonEl         10 0
163700010601     C                   z-add     *zeros        depfil            3 0
163800010601     C                   z-add     *zeros        invfil            3 0
163900010601     C*
164000010601     C                   ENDSR
164100010601     C***
164200070326
164300070326
164400070326
164500070326
164600070326     C     *pssr         BEGSR
164700070329     C*
164800070329     C                   if        %open(tivin00r)
164900070329     C                   close     tivin00r
165000070329     C                   endif
165100070329     C                   if        %open(fivabwwr)
165200070329     C                   close     fivabwwr
165300070329     C                   endif
165400070329     C                   if        %open(fivatwwr)
165500070329     C                   close     fivatwwr
165600070329     C                   endif
165700070326     C*
165800070326     C* Effettuo la chiamata al CLLE preposto
165900070326     C                   call(e)   'TITVVTC'
166000070326     C                   parm                    parccm
166100070326     C                   parm                    parmbr
166200070326     C                   parm      '2'           paropz
166300070326     C*
166400070326     C                   eval      wrkesito = '2'
166500070404     C*
166600070404     C                   seton                                        LR
166700070326     C*
166800070326     C                   ENDSR     '*CANCL'
166900070326     C***
167000100318
167100100318
167200100318
167300100318     C     normCSV       BEGSR
167400100318     C*
167500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
167600100318     C* separatori campi
167700100318     C                   if        CharTXT <> *blanks
167800100318     C                   z-add     *zeros        wLenVINDTA        4 0
167900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
168000100318     C                   setoff                                       80
168100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
168200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
168300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
168400100318     C                   if        not *in80
168500100318     C                   seton                                        80
168600100318     C                   else
168700100318     C                   setoff                                       80
168800100318     C                   endif
168900100318     C                   endif
169000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
169100100318     C                             and *in80 = *on
169200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
169300100318     C                   endif
169400100318     C                   enddo
169500100318     C                   endif
169600100318     C*
169700100318     C                   ENDSR
169800100318     C***
169900070326
170000100326
170100100326
170200100326     C*----------------------------------------------------*
170300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
170400100326     C*----------------------------------------------------*
170500100326     C     REPNSP        BEGSR
170600100326     C*
170700100326     C* NSP => Stacco un numeratore da AZNUM
170800100326     C                   clear                   TRUL33DS
170900100326     C                   eval      I33OPE = *zeros
171000100326     C                   eval      I33CNU = 302
171100100326     C                   eval      I33NUM = 1
171200100326     C                   movel     TRUL33DS      KPJBU
171300100326     C                   call      'TRUL33R'
171400100326     C                   parm                    KPJBA
171500100326     C                   movel     KPJBU         TRUL33DS
171600100326     C                   if        O33ERR = *zeros
171700100326     C                   z-add     O33NRF        VABNSP
171800100326     C                   z-add     O33NRF        VATNSP
171900100326     C                   else
172000100326     C                   SETON                                        31
172100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
172200100326     C                             + ' ' + 'VABNSP VATNSP'
172300100326     C                   endif
172400100326     C*
172500100326     C                   ENDSR
172600010330
172700100326
172800010330
172900000613     C     *inzsr        BEGSR
173000990910     C*
173100990910     C     *entry        plist
173200990920     C                   parm                    tivlrds
173300990921     C                   parm      wrkesito      esito
173400000724     C                   parm                    prmlit
173500000710     C                   parm                    prmfir
173600010330     C*
173700010330     C* CALCOLA LA DATA CORRENTE
173800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
173900100318     C                   eval      datcor = %dec(%date() : *ISO)
174000000613     C*
174100000613     C                   ENDSR
174200000613     C***
174300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
174400100921VABRMN;VABNCL;VABPKB;W_VABVLB;W_VABRMA;VABRSD;VABIND;VABLOD;VABCAD;
174500100921VATNOT_A;VATNOT_B;FIL001;W_VABCBO;VABPRD;W_VABNOT_1;W_VABNOT_2;
174600100921VABDCR;
174700100318
174800100318
