000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500080527     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
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
002000080527     D fivabds       e ds                  extname(fivab00f)
002100080527     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002200080527     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
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
012800000613     C*
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
030900080527     C     CHKVAB        BEGSR
031000080527     C*
031100080527     C* Scrivo il VAB solo a rottura d spedizione
031200080527     C                   if        depspe = *zeros
031300080527     C  N31              eval      fivabds_sav = fivabds
031400080527     C                   eval      depspe = vabNSP
031500080527     C                   else
031600080527     C                   if        depspe = vabNSP AND *in65 = *off
031700080527     C                   add       VABNCL        sav_VABNCL
031800080527     C                   add       VABPKB        sav_VABPKB
031900080527     C                   else
032000080527     C  N31              eval      fivabds_ok = fivabds_sav
032100080527     C  N31              exsr      wrivab
032200080527     C  N31              eval      fivabds_sav = fivabds
032300080527     C                   eval      depspe = vabNSP
032400080527     C                   endif
032500080527     C                   endif
032600080527     C*
032700080527     C                   ENDSR
032800040119
032900040119
033000040119
033100040119     C*----------------------------------------------------*
033200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
033300040119     C*----------------------------------------------------*
033400040510     C     WRIVAB        BEGSR
033500080513     C*
033600080513     C* Gestisco l'eventuale rottura x numero spedizione
033700080527     C                   if        VABNSP <> depspe or *in65 = *on
033800070301     C                   if        wVATNOT_E <> *blanks
033900070301     C                   eval      VABCTM = '7Q'
034000070301     C                   endif
034100040119     C                   write     fivab000                                     => scarico il VAB
034200080513     C                   endif
034300040119     C*
034400040119     C                   ENDSR
034500040119
034600040119
034700040119
034800040119     C*----------------------------------------------------*
034900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
035000040119     C*----------------------------------------------------*
035100040510     C     WRIVAT        BEGSR
035200060512     C*
035300060512     C* Inizializzo i campi chiave
035400060512     C                   eval      VATFGS = VABFGS
035500060512     C                   eval      VATAAS = VABAAS
035600060512     C                   eval      VATCCM = VABCCM
035700070823     C                   eval      VATNRS = VABNRS
035800060512     C                   eval      VATNSP = VABNSP
035900060512     C                   eval      VATLNP = VABLNP
036000040714     C*
036100040714     C* Solo se VATNOT valorizzato scarico il buffer
036200040714     C                   if        VATNOT <> *blanks
036300040119     C*
036400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
036500040510     C                   if        VATTRC = *blanks
036600040510     C                   eval      VATTRC = 'E'
036700040510     C                   endif
036800040510     C                   write     FIVAT000
036900040119     C                   add       1             §CTROKVT
037000040714     C*
037100040714     C                   endif
037200060331     C*
037300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
037400060331     C                   if        wVATNOT_A <> *blanks
037500060331     C                   eval      VATNOT = wVATNOT_A
037600060331     C                   eval      VATTRC = 'A'
037700060331     C                   write     FIVAT000
037800060331     C                   add       1             §CTROKVT
037900060331     C                   endif
038000060331     C*
038100060331     C                   if        wVATNOT_B <> *blanks
038200060331     C                   eval      VATNOT = wVATNOT_B
038300060331     C                   eval      VATTRC = 'B'
038400060331     C                   write     FIVAT000
038500060331     C                   add       1             §CTROKVT
038600060331     C                   endif
038700060331     C*
038800060331     C                   if        wVATNOT_E <> *blanks
038900060331     C                   eval      VATNOT = wVATNOT_E
039000060331     C                   eval      VATTRC = 'E'
039100060331     C                   write     FIVAT000
039200060331     C                   add       1             §CTROKVT
039300060331     C                   endif
039400040119     C*
039500040119     C                   ENDSR
039600040119
039700040119
039800040119
039900040119     C*----------------------------------------------------*
040000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040100040119     C*----------------------------------------------------*
040200040119     C     PREVAT        BEGSR
040300040119     C*
040400040119     C* Compongo il nome del membro da dare al FIVATWWR
040500040119     C                   eval      parmbr = vlrhdl
040600040119     C                   movel     'M'           parmbr
040700040510     C                   eval      parccm = vlrksc
040800040119     C                   eval      paropz = '1'
040900040119     C* Effettuo la chiamata al CLLE preposto
041000040119     C                   call(e)   'TITVVTC'
041100040119     C                   parm                    parccm
041200040119     C                   parm                    parmbr
041300040119     C                   parm                    paropz
041400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041500040119     C                   if        %error
041600040119     C                   movel     '1'           chkcall
041700040119     C                   else
041800040119     C                   movel     '0'           chkcall
041900040119     C                   endif
042000040119     C*
042100040119     C                   ENDSR
042200010601
042300010601
042400010601
042500010330     C*----------------------------------------------------*
042600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042700010330     C*----------------------------------------------------*
042800040119     C     INZVAR        BEGSR
042900080527     C*
043000080527     C                   setoff                                       65
043100010330     C*
043200030822     C* Inizializzo variabili di wrk
043300020204     C                   Z-ADD     *zeros        Num5_0            5 0
043400060331     C                   MOVEL     *blanks       wVATNOT_A
043500060331     C                   MOVEL     *blanks       wVATNOT_B
043600060331     C                   MOVEL     *blanks       wVATNOT_E
043700030822     C*
043800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
043900030822     C                   CLEAR                   SkSplitCSV
044000030822     C                   EVAL      i = 1
044100030822     C                   EVAL      posDa = *zeros
044200030822     C                   EVAL      posA  = *zeros
044300080527     C*
044400080527     C                   CLEAR                   FIVABDS
044500080527     C                   CLEAR                   FIVAT000
044600030822     C*
044700030822     C* Reimposto i valori di default
044800030822     C                   EXSR      DEFCAM
044900030822     C*
045000010330     C                   ENDSR
045100010330     C*----------------------------------------------------*
045200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
045300010330     C*----------------------------------------------------*
045400010330     C     DEFCAM        BEGSR
045500030822     C*
045600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
045700030715     C* e delimitatore testo.
045800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
045900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046100100318     C*
046200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
046300100318     C                   EVAL      CharSOS = CharNUM
046400030822     C*
046500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
046600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
046700030715     C                   SETON                                        50
046800030715     C                   ELSE
046900030715     C                   SETOFF                                       50
047000030715     C                   ENDIF
047100030822     C*
047200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
047300030822     C                   EVAL      posDaDft = 1
047400030822     C                   EVAL      posADft  = 0
047500030822     C                   EVAL      wGiroDft = 0
047600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047700030822     C                             posDaDft > 0
047800030822     C*
047900030822     C* Gestisco il 1° giro
048000030822     C                   IF        wGiroDft = 0
048100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
048200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
048300030822     C* Incremento il contatore dei "giri"
048400030822     C                   EVAL      wGiroDft = 1
048500030822     C                   ELSE
048600030822     C                   EVAL      posDaDft = posADft
048700030822     C                   ENDIF
048800030822     C* Eseguo lo scan x trovare la fine del campo corrente
048900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
049000030822     C*
049100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
049200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
049300030822     C                   IF        posDaDft > 0 AND
049400030822     C                             posADft  > 0
049500030822     C* NCL
049600030822     C                   IF        %subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):1:3)
049900030822     C                             = 'NCL'
050000030822     C                   EVAL      PiStr=%trim(%subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050300030822     C                   EXSR      CHKNUM
050400030822     C                   IF        PiInt=*on
050500030822     C                   Z-ADD     PiVal         VABNCL
050600030822     C                   ENDIF
050700030822     C                   ENDIF
050800030822     C* CCM
050900030822     C                   IF        %subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):1:3)
051200030822     C                             = 'CCM'
051300030822     C                   EVAL      PiStr=%trim(%subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600030822     C                   EXSR      CHKNUM
051700030822     C                   IF        PiInt=*on
051800030822     C                   Z-ADD     PiVal         VABCCM
051900030822     C                   ENDIF
052000030822     C                   ENDIF
052100030822     C* LNP
052200030822     C                   IF        %subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):1:3)
052500030822     C                             = 'LNP'
052600030822     C                   EVAL      PiStr=%trim(%subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900030822     C                   EXSR      CHKNUM
053000030822     C                   IF        PiInt=*on
053100030822     C                   Z-ADD     PiVal         VABLNP
053200030822     C                   ENDIF
053300030822     C                   ENDIF
053400030822     C* NRS
053500030822     C                   IF        %subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):1:3)
053800030822     C                             = 'NRS'
053900030822     C                   EVAL      PiStr=%trim(%subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200030822     C                   EXSR      CHKNUM
054300030822     C                   IF        PiInt=*on
054400030822     C                   Z-ADD     PiVal         VABNRS
054500030822     C                   ENDIF
054600030822     C                   ENDIF
054700030822     C* CTR
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'CTR'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABCTR
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* PKB
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'PKB'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiNum=*on
057000030822     C                   Z-ADD     PiVal         VABPKB
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* VLB
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'VLB'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiNum=*on
058300030822     C                   Z-ADD     PiVal         VABVLB
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* QFT
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'QFT'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiNum=*on
059600030822     C                   Z-ADD     PiVal         VABQFT
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* CBO
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'CBO'
060400030822     C                   EVAL      VABCBO=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   ENDIF
060800030822     C* TSP
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'TSP'
061300030822     C                   EVAL      VABTSP=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   ENDIF
061700030822     C* VAS
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'VAS'
062200030822     C                   EVAL      VABVAS=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   ENDIF
062600030822     C* VCA
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'VCA'
063100030822     C                   EVAL      VABVCA=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   ENDIF
063500070925     C* TIC
063600070925     C                   IF        %subst(
063700070925     C                             %subst(vlrppt:posDaDft+1:
063800070925     C                             posADft-posDaDft-1):1:3)
063900070925     C                             = 'TIC'
064000070925     C                   EVAL      VABTIC=%trim(%subst(
064100070925     C                             %subst(vlrppt:posDaDft+1:
064200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300070925     C                   ENDIF
064400030822     C* GCA
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'GCA'
064900030822     C                   EVAL      VABGCA=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   ENDIF
065300030822     C* CTM
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'CTM'
065800030822     C                   EVAL      VABCTM=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* FFD
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'FFD'
066700030822     C                   EVAL      VABFFD=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100030822     C* VAD
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'VAD'
067600030822     C                   EVAL      VABVAD=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* GMA
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'GMA'
068500030822     C                   EVAL      VABGMA=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* GGA
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'GGA'
069400030822     C                   EVAL      VABGGA=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* GVA
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'GVA'
070300030822     C                   EVAL      VABGVA=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* TC1
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'TC1'
071200030822     C                   EVAL      VABTC1=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* TC2
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'TC2'
072100030822     C                   EVAL      VABTC2=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500120320     C* GC1
072600120320     C                   IF        %subst(
072700120320     C                             %subst(vlrppt:posDaDft+1:
072800120320     C                             posADft-posDaDft-1):1:3)
072900120320     C                             = 'GC1'
073000120320     C                   EVAL      VABGC1=%trim(%subst(
073100120320     C                             %subst(vlrppt:posDaDft+1:
073200120320     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300120320     C                   ENDIF
073400040714     C* VATTRC
073500040714     C                   IF        %subst(
073600040714     C                             %subst(vlrppt:posDaDft+1:
073700040714     C                             posADft-posDaDft-1):1:3)
073800040714     C                             = 'TRC'
073900040714     C                   EVAL      VATTRC=%trim(%subst(
074000040714     C                             %subst(vlrppt:posDaDft+1:
074100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200040714     C                   ENDIF
074300030822     C* ...
074400030822     C                   ENDIF
074500030822     C                   ENDDO
074600020204     C*
074700010330     C                   ENDSR
074800010607     C*----------------------------------------------------*
074900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075000010607     C*----------------------------------------------------*
075100010607     C     IMPVAB        BEGSR
075200010607     C*
075300010607     C                   SETOFF                                       3132
075400030515     C*
075500010607     C                   MOVEL     datcor        VABAAS
075600010607     C                   MOVE      datcor        VABMGS
075700010607     C*
075800030715     C**********
075900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076000030715     C**********
076100030715     C                   IF        wGiro = *zeros
076200040113     C*
076300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
076400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
076500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
076600080429     C                             %trim(intcol(5))<>*blanks
076700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
076800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
076900080429     C                                      %trim(intcol(5))
077000070823     C     ';':CharCSV   XLATE     vindta        vindta
077100040510     C                   ENDIF
077200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077300031201     C     minu:maiu     XLATE     vindta        vindta
077400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077500030715     C                   DOW       posDa <= %len(%trim(vindta))
077600030715     C*
077700030715     C* Gestisco il 1° campo
077800030715     C                   IF        i = 1
077900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078000030715     C                   EVAL      posDa = 1
078100030715     C* Eseguo lo scan x trovare la fine del primo campo
078200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078300030715     C* A questo punto "estraggo" il campo corrente
078400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078500030715     C                                                    (posA-posDa))
078600030715     C* X i campi successivi al 1°
078700030715     C                   ELSE
078800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078900030715     C                   EVAL      posDa = posA + 1
079000030715     C* Eseguo lo scan x trovare la fine del campo corrente
079100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079200030715     C* Gestisco l'ultimo campo
079300030715     C                   IF        posA = *zeros
079400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
079500030715     C* A questo punto "estraggo" il campo corrente
079600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079700030715     C                                                    (posA-posDa))
079800030715     C                   LEAVE
079900030715     C                   ELSE
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C                   ENDIF
080400030715     C                   ENDIF
080500030715     C* Incremento il contatore d campo
080600030715     C                   EVAL      i = i +1
080700030715     C                   ENDDO
080800030715     C*
080900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081000030715     C* delimitazione testo
081100030715     C                   EVAL      i = 1
081200030715     C                   DOW       i < %elem(SkSplitFLD)
081300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081500030715     C                   EVAL      i = i + 1
081600030715     C                   ENDDO
081700030715     C                   EVAL      wGiro = 1
081800030715     C                   ELSE
081900030715     C                   EVAL      wGiro = 2
082000031204     C**********
082100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082200031204     C* d separatore campo
082300031204     C**********
082400031204     C                   EVAL      vindta = %trim(vindta)
082500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
082600031204     C                   EVAL      vindta = %subst(vindta:2)
082700031204     C                   ENDDO
082800031204     C*
082900031204     C                   Z-ADD     *zeros        lunghInput        4 0
083000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083400031204     C                   ENDDO
083500030715     C**********
083600030715     C* Effettuo lo split del campo dati d input
083700030715     C**********
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
084800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084900030715     C                                                    (posA-posDa))
085000030715     C* X i campi successivi al 1°
085100030715     C                   ELSE
085200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085300030715     C                   EVAL      posDa = posA + 1
085400031204     C* Verifico che nn vi sia il campo nn valorizzato
085500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
085600031204     C* Se campo nn valorizzato skippo al prossimo
085700031204     C                   EVAL      SkSplitCSV(i) = *blanks
085800031204     C                   EVAL      posA  = posA + 1
085900031204     C                   ELSE
086000030715     C* Eseguo lo scan x trovare la fine del campo corrente
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* Gestisco l'ultimo campo
086300030715     C                   IF        posA = *zeros
086400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C                   LEAVE
086900030715     C                   ELSE
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C                   ENDIF
087400031204     C                   ENDIF
087500030715     C                   ENDIF
087600030715     C* Incremento il contatore d campo
087700030715     C                   EVAL      i = i +1
087800030715     C                   ENDDO
087900030715     C*
088000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088100030715     C* delimitazione testo
088200030715     C                   EVAL      i = 1
088300030715     C                   DOW       i < %elem(SkSplitCSV)
088400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
088500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
088600030715     C                   EVAL      i = i + 1
088700030715     C                   ENDDO
088800100326     C*
088900100326     C* Stacco un progressivo x numero spedizione
089000100326     C                   EXSR      repNSP
089100100326     C*
089200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089300030715     C                   EVAL      i = 1
089400030715     C                   DOW       i < %elem(SkSplitFLD)
089500030715     C*
089600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
089800030715     C                             SkSplitCSV(i) <> *blanks
089900030721     C*** ATB
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090100030721     C                   EVAL      VABATB = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030715     C*** RSD
090400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
090800030715     C* ==
090900030715     C                   ENDIF
091000030721     C*** RD2
091100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091300030721     C                   ENDIF
091400030715     C*** IND
091500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091600030715     C                   EVAL      VABIND = SkSplitCSV(i)
091700030715     C                   ENDIF
091800030715     C*** LOD
091900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092100030715     C                   ENDIF
092200030715     C*** PRD
092300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092500030715     C                   ENDIF
092600030721     C*** NZD
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030715     C*** RMA
093100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093300030715     C                   ENDIF
093400030715     C*** NOT
093500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093700030715     C                   ENDIF
093800030715     C*** NT2
093900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094100030715     C                   ENDIF
094200030715     C*** VCA
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094500030715     C                   ENDIF
094600030715     C*** VAS
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** FFD
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030721     C*** GC1
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030721     C*** GC2
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030721     C*** TSP
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096500030721     C                   ENDIF
096600040510     C*** CBO
096700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
096800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
096900040510     C                   ENDIF
097000030721     C*** NAS
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** TIC
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** GCA
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** XCO
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** CTM
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
098800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** TCR
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** CTS
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** FTM
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** VAD
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** GMA
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
100800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** GGA
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** GVA
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TC1
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** TC2
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** SCL
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
102800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** RMO
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** NMO
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*
103900030721     C* Reperisco quindi i campi numerici...
104000040510     C*** NSP
104100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104200040510     C                   EVAL      PiStr=SkSplitCSV(i)
104300040510     C                   EXSR      CHKNUM
104400040510     C                   IF        PiInt=*on
104500040510     C                   Z-ADD     PiVal         VABNSP
104600040510     C                   ELSE
104700040510     C                   SETON                                        31
104800040510     C                   EVAL      VABNSP = *zeros
104900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
105000040510     C                             + ' ' + 'VABNSP'
105100040510     C                   ENDIF
105200040510     C                   ENDIF
105300071106     C*** RMN
105400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105500071106     C                   EVAL      PiStr=SkSplitCSV(i)
105600071106     C                   EXSR      CHKNUM
105700071106     C                   IF        PiInt=*on
105800071106     C                   Z-ADD     PiVal         VABRMN
105900071106     C                   ELSE
106000071106     C                   SETON                                        32
106100071106     C                   EVAL      VABRMN = *zeros
106200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
106300071106     C                             + ' ' + 'VABRMN'
106400071106     C                   ENDIF
106500071106     C                   ENDIF
106600030721     C*** CAD
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106800030721     C                   EVAL      PiStr=SkSplitCSV(i)
106900030721     C                   EXSR      CHKNUM
107000030721     C                   IF        PiInt=*on
107100030721     C                   Z-ADD     PiVal         Num5_0
107200030721     C                   MOVEL(p)  Num5_0        VABCAD
107300030721     C                   ELSE
107400030721     C                   SETON                                        32
107500030721     C                   EVAL      VABCAD = *zeros
107600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107700030721     C                             + ' ' + 'VABCAD'
107800030721     C                   ENDIF
107900030721     C                   ENDIF
108000030721     C*** NCL
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108200030721     C                   EVAL      PiStr=SkSplitCSV(i)
108300030721     C                   EXSR      CHKNUM
108400030721     C                   IF        PiInt=*on
108500030721     C                   Z-ADD     PiVal         VABNCL
108600030721     C                   ELSE
108700030721     C                   SETON                                        32
108800030721     C                   EVAL      VABNCL = *zeros
108900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109000040122     C                             + ' ' + 'VABNCL'
109100030721     C                   ENDIF
109200030721     C                   ENDIF
109300030721     C*** PKB
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiNum=*on
109800030721     C                   Z-ADD     PiVal         VABPKB
109900030721     C                   ELSE
110000030721     C                   SETON                                        32
110100030721     C                   EVAL      VABPKB = *zeros
110200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110300030721     C                             + ' ' + 'VABPKB'
110400030721     C                   ENDIF
110500030721     C                   ENDIF
110600030721     C*** CAS
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiNum=*on
111100030721     C                   Z-ADD     PiVal         VABCAS
111200030721     C                   ELSE
111300030721     C                   SETON                                        32
111400030721     C                   EVAL      VABCAS = *zeros
111500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111600030721     C                             + ' ' + 'VABCAS'
111700030721     C                   ENDIF
111800030721     C                   ENDIF
111900030721     C*** CCM
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiInt=*on      AND
112400030721     C                             PiVal<=9999999 AND
112500030721     C                             PiVal>*zeros
112600030721     C                   Z-ADD     PiVal         VABCCM
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABCCM = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040510     C                             + ' ' + 'VABCCM'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** LNP
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on    AND
113900030721     C                             PiVal<=999   AND
114000030721     C                             PiVal>*zeros
114100030721     C                   Z-ADD     PiVal         VABLNP
114200030721     C                   ELSE
114300040715     C                   SETON                                        32
114400030721     C                   EVAL      VABLNP = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600040510     C                             + ' ' + 'VABLNP'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** NRS
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiInt=*on AND
115400030721     C                             PiVal<=99
115500030721     C                   Z-ADD     PiVal         VABNRS
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABNRS'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** LNA
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiInt=*on    AND
116700030721     C                             PiVal<=999   AND
116800030721     C                             PiVal>*zeros
116900030721     C                   Z-ADD     PiVal         VABLNA
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABLNA = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABLNA'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** CTR
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on  AND
118200030721     C                             PiVal<=999
118300030721     C                   Z-ADD     PiVal         VABCTR
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABCTR'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** IAS
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiNum=*on
119500030721     C                   Z-ADD     PiVal         VABIAS
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABIAS'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** VLB
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiNum=*on
120700030721     C                   Z-ADD     PiVal         VABVLB
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABVLB'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** QFT
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiNum=*on
121900030721     C                   Z-ADD     PiVal         VABQFT
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABQFT'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** NCD
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on      AND
123100030721     C                             PiVal<=9999999
123200030721     C                   Z-ADD     PiVal         VABNCD
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABNCD'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** NCA
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on      AND
124400030721     C                             PiVal<=9999999
124500030721     C                   Z-ADD     PiVal         VABNCA
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABNCA'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** ZNC
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on AND
125700030721     C                             PiVal<=99
125800030721     C                   Z-ADD     PiVal         VABZNC
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABZNC'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** DCR
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiInt=*on       AND
127000030721     C                             PiVal<=99999999
127100030721     C                   Z-ADD     PiVal         VABDCR
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABDCR'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** HCR
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on   AND
128300030721     C                             PiVal<=9999
128400030721     C                   Z-ADD     PiVal         VABHCR
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABHCR'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** VMD
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiNum=*on
129600030721     C                   Z-ADD     PiVal         VABVMD
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABVMD'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** ANT
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiInt=*on        AND
130800030721     C                             PiVal<=999999999
130900030721     C                   Z-ADD     PiVal         VABANT
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABANT'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** CMO
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on
132100030721     C                   Z-ADD     PiVal         Num5_0
132200030721     C                   MOVEL(p)  Num5_0        VABCMO
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      VABCMO = *zeros
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABCMO'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000040510     C*
133100040510     C* Reperisco i campi del FIVAT
133200040510     C*** VATNOT
133300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133500040510     C                   ENDIF
133600040510     C*** VATTRC
133700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133900040510     C                   ENDIF
134000060331     C*** VATNOT_A
134100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
134200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134300060331     C                   ENDIF
134400060331     C*** VATNOT_B
134500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134700060331     C                   ENDIF
134800060331     C*** VATNOT_E
134900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
135000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
135100060331     C                   ENDIF
135200070823     C*
135300071029     C***  ===>  Gestione campi "particolari" dentro ciclo
135400100326     C*** RMN/RMA
135500100326     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
135600080513     C                   EVAL      VABRMA=SkSplitCSV(i)
135700080513     C                   EVAL      PiStr=SkSplitCSV(i)
135800080513     C                   EXSR      CHKNUM
135900080513     C                   IF        PiInt=*on
136000100318     C                   Z-ADD     PiVal         VABRMN
136100080513     C                   ELSE
136200080513     C                   SETON                                        31
136300100318     C                   EVAL      VABRMN = *zeros
136400080513     C                   EVAL      vinmsg = %trimr(vinmsg)
136500100326     C                             + ' ' + 'VABRMN VABRMA'
136600080513     C                   ENDIF
136700080513     C                   ENDIF
136800100326     C*** NOT/NT2
136900100326     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
137000100326     C                   EVAL      VABNOT=%subst(SkSplitCSV(i):01:35)
137100100326     C                   EVAL      VABNT2=%subst(SkSplitCSV(i):36:35)
137200100326     C                   ENDIF
137300070925     C*
137400071029     C***  <===  -----------------------------------------
137500040510     C*
137600030715     C                   ENDIF
137700030715     C                   EVAL      i = i + 1
137800030715     C                   ENDDO
137900070925     C*
138000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
138100070925     C*
138200071029     C***  <===  -----------------------------------------
138300070925     C*
138400070925     C* Considerazioni finali su CBO/CAS
138500100318     C                   IF        wFlgCAS = '1'    OR
138600100318     C                             VABCAS  > *zeros
138700070925     C                   IF        VABCBO = '1'
138800070925     C                   EVAL      VABCBO = '4'
138900070925     C                   ENDIF
139000070925     C                   IF        VABCBO = '2'
139100070925     C                   EVAL      VABCBO = '6'
139200070925     C                   ENDIF
139300070925     C                   ENDIF
139400070925     C***  <===  ----------------------------
139500040510     C*
139600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
139700040510     C                   IF        VABNSP = *zeros
139800040510     C                   SETON                                        31
139900040510     C                   ENDIF
140000040715     C*
140100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
140200040715     C                   IF        VABLNP = *zeros
140300040715     C                   SETON                                        31
140400040715     C                   ENDIF
140500040510     C*
140600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
140700040510     C                   IF        VABRMN = *zeros
140800040510     C                   Z-ADD     VABNSP        VABRMN
140900040510     C                   ENDIF
141000040510     C*
141100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
141200040510     C                   IF        VABRMA = *blanks
141300040714     C                   MOVEL     VABRMN        VABRMA
141400040510     C                   ENDIF
141500040128     C*
141600040510     C* Se provincia nn valorizzata la reperisco
141700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
141800040510     C                   IF        VABPRD = *blanks
141900040128     C                   CLEAR                   TISI95DS
142000040128     C                   EVAL      I95TCN = '3'
142100040128     C                   Z-ADD     datcor        I95DAT
142200040128     C                   EVAL      I95NAR = VABNZD
142300040128     C                   EVAL      I95CAP = VABCAD
142400040128     C                   EVAL      I95LOC = VABLOD
142500040128     C                   CALL      'TISI95R'
142600040128     C                   PARM                    TISI95DS
142700040128     C                   EVAL      VABPRD = O95PRV
142800040510     C                   ENDIF
142900020204     C*
143000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
143100020204     C                   EXSR      CHKIMPDIV
143200030715     C                   ENDIF
143300020204     C*
143400010607     C                   ENDSR
143500010607     C*----------------------------------------------------*
143600010601
143700020204
143800020204     C*----------------------------------------------------*
143900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
144000020204     C*----------------------------------------------------*
144100020204     C     CHKIMPDIV     BEGSR
144200020204     C*
144300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
144400020204     C                   Z-ADD     *zeros        wrkDec            9 9
144500020204     C*
144600020204     C* Come prima cosa effettuo considerazioni sulla divisa
144700020204     C                   IF        vabIAS > *zeros
144800020204     C                   IF        vabVAS <> 'EUR'
144900020204     C                   EVAL      vabVAS =  'ITL'
145000020204     C                   ENDIF
145100020204     C                   ENDIF
145200020204     C*
145300020204     C                   IF        vabCAS > *zeros
145400020204     C                   IF        vabVCA <> 'EUR'
145500020204     C                   EVAL      vabVCA =  'ITL'
145600020204     C                   ENDIF
145700020204     C                   ENDIF
145800020204     C*
145900020204     C                   IF        vabVMD > *zeros
146000020204     C                   IF        vabVAD <> 'EUR'
146100020204     C                   EVAL      vabVAD =  'ITL'
146200020204     C                   ENDIF
146300020204     C                   ENDIF
146400020204     C*
146500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
146600020204     C                   Z-ADD     vabIAS        wrkDec
146700020204     C                   IF        wrkDec > *zeros
146800020204     C                   IF        vabVAS = 'ITL'
146900020204     C                   EVAL      vabIAS = *zeros
147000020204     C                   ENDIF
147100020204     C                   ENDIF
147200020204     C*
147300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
147400020204     C                   Z-ADD     vabCAS        wrkDec
147500020204     C                   IF        wrkDec > *zeros
147600020204     C                   IF        vabVCA = 'ITL'
147700020204     C                   EVAL      vabCAS = *zeros
147800020204     C                   ENDIF
147900020204     C                   ENDIF
148000020204     C*
148100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
148200020204     C                   Z-ADD     vabVMD        wrkDec
148300020204     C                   IF        wrkDec > *zeros
148400020204     C                   IF        vabVAD = 'ITL'
148500020204     C                   EVAL      vabVMD = *zeros
148600020204     C                   ENDIF
148700020204     C                   ENDIF
148800020204     C*
148900020204     C                   ENDSR
149000020204     C***
149100010330
149200100318
149300100318
149400100318     C*----------------------------------------------------*
149500100318     C*  CONTROLLO NUMERICITA' CAMPI
149600100318     C*----------------------------------------------------*
149700100318     C     CHKNUM        BEGSR
149800100318     C*
149900100318     C                   IF        PiDecChr = *blanks
150000100318     C                   EVAL      PiDecChr = CharNUM
150100100318     C                   ENDIF
150200100318     C*
150300100318     C                   callp     UBISNUM_Check(PiStr
150400100318     C                                          :PiDecChr
150500100318     C                                          :PiVal
150600100318     C                                          :PiNum
150700100318     C                                          :PiInt)
150800100318     C*
150900100318     C                   ENDSR
151000100318     C***
151100010601
151200010601
151300010601
151400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
151500010601     C     repfil        BEGSR
151600010601     C*
151700010601     C                   if        invfil = *zeros and
151800010601     C                             depfil > *zeros and
151900010629     C                             (vinflg = *blanks or
152000010629     C                              vinflg = *zeros)
152100010601     C
152200010601     C                   eval      invfil = depfil
152300010601     C                   endif
152400010601     C*
152500010601     C                   if        depfil <> invfil and
152600010601     C                             invfil > *zeros
152700010601     C                   eval      flgMulti = '1'
152800010601     C                   if        vinflg = *blanks
152900010601     C                   add       1             cntNonEl
153000010601     C                   endif
153100010601     C                   endif
153200010601     C*
153300010601     C                   if        vinflg = '2'
153400010601     C                   eval      flgStato = '2'
153500010601     C                   endif
153600010601     C*
153700010601     C                   ENDSR
153800010601     C***
153900010601
154000010601
154100010601
154200010330
154300040119      /TITLE Invio dei dati al punto operativo.
154400040119     C     invio         BEGSR
154500040119     C*
154600040119     C* 1° invio FIVAT
154700040119     C                   reset                   dscmz
154800040119     C                   move      vlrpoi        cmzdst
154900040119     C                   eval      cmzfld = 'FIVATWWR'
155000040119     C                   eval      cmzmbd = vlrhdl
155100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155200040119     C***                if        prmfir = *blanks
155300040119     C                   eval      cmzfla = 'FIVAT00F'
155400040119     C                   eval      cmzmba = 'FIVAT00F'
155500040119     C***                else
155600040119     C***                eval      cmzfla = prmfir
155700040119     C***                eval      cmzmba = prmfir
155800040119     C***                endif
155900040119     C                   eval      cmznrr = *zeros
156000040119     C                   move      §ctrokvt      cmznrr
156100040119     C                   eval      cmzlba = vlrfl1
156200040119     C                   call(e)   'TIS711C'
156300040119     C                   parm                    dscmz
156400040119     C                   parm      *blanks       esito
156500040119     C                   if        %error
156600040119     C                             or cmzerr = '1'
156700040119     C                             or esito  = '1'
156800040119     C                   eval      wrkesito = '3'
156900040119     C                   else
157000040119     C*
157100040119     C* 2° invio FIVAB
157200040119     C                   reset                   dscmz
157300040119     C                   move      vlrpoi        cmzdst
157400040119     C                   eval      cmzfld = vlrfou
157500040119     C                   eval      cmzmbd = vlrhdl
157600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
157700040119     C***                if        prmfir = *blanks
157800040119     C                   eval      cmzfla = 'FIVAB00F'
157900040119     C                   eval      cmzmba = 'FIVAB00F'
158000040119     C***                else
158100040119     C***                eval      cmzfla = prmfir
158200040119     C***                eval      cmzmba = prmfir
158300040119     C***                endif
158400040119     C                   eval      cmznrr = *zeros
158500040119     C                   move      §ctrokvb      cmznrr
158600040119     C                   eval      cmzlba = vlrfl1
158700040119     C                   call(e)   'TIS711C'
158800040119     C                   parm                    dscmz
158900040119     C                   parm      *blanks       esito
159000040119     C                   if        %error
159100040119     C                             or cmzerr = '1'
159200040119     C                             or esito  = '1'
159300040119     C                   eval      wrkesito = '3'
159400040119     C                   endif
159500040119     C                   endif
159600040119     C*
159700040119     C                   ENDSR
159800040119     C***
159900010601
160000010601
160100010601
160200010601
160300010601      /TITLE Invio dei dati al punto operativo.
160400010601     C     opeini        BEGSR
160500010601     C*
160600010601     C* Inizializzo flag e contatori operativi
160700010601     C                   movel     '0'           flgGiro           1
160800010601     C                   movel     '0'           flgMulti          1
160900010601     C                   movel     '1'           flgStato          1
161000010615     C                   movel     '0'           flgOk             1
161100010601     C                   z-add     *zeros        cntNonEl         10 0
161200010601     C                   z-add     *zeros        depfil            3 0
161300010601     C                   z-add     *zeros        invfil            3 0
161400010601     C*
161500010601     C                   ENDSR
161600010601     C***
161700070326
161800070326
161900070326
162000070326
162100070326     C     *pssr         BEGSR
162200070329     C*
162300070329     C                   if        %open(tivin00r)
162400070329     C                   close     tivin00r
162500070329     C                   endif
162600070329     C                   if        %open(fivabwwr)
162700070329     C                   close     fivabwwr
162800070329     C                   endif
162900070329     C                   if        %open(fivatwwr)
163000070329     C                   close     fivatwwr
163100070329     C                   endif
163200070326     C*
163300070326     C* Effettuo la chiamata al CLLE preposto
163400070326     C                   call(e)   'TITVVTC'
163500070326     C                   parm                    parccm
163600070326     C                   parm                    parmbr
163700070326     C                   parm      '2'           paropz
163800070326     C*
163900070326     C                   eval      wrkesito = '2'
164000070404     C*
164100070404     C                   seton                                        LR
164200070326     C*
164300070326     C                   ENDSR     '*CANCL'
164400070326     C***
164500100318
164600100318
164700100318
164800100318     C     normCSV       BEGSR
164900100318     C*
165000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
165100100318     C* separatori campi
165200100318     C                   if        CharTXT <> *blanks
165300100318     C                   z-add     *zeros        wLenVINDTA        4 0
165400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
165500100318     C                   setoff                                       80
165600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
165700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
165800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
165900100318     C                   if        not *in80
166000100318     C                   seton                                        80
166100100318     C                   else
166200100318     C                   setoff                                       80
166300100318     C                   endif
166400100318     C                   endif
166500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
166600100318     C                             and *in80 = *on
166700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
166800100318     C                   endif
166900100318     C                   enddo
167000100318     C                   endif
167100100318     C*
167200100318     C                   ENDSR
167300100318     C***
167400070326
167500100326
167600100326
167700100326     C*----------------------------------------------------*
167800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
167900100326     C*----------------------------------------------------*
168000100326     C     REPNSP        BEGSR
168100100326     C*
168200100326     C* NSP => Stacco un numeratore da AZNUM
168300100326     C                   clear                   TRUL33DS
168400100326     C                   eval      I33OPE = *zeros
168500100326     C                   eval      I33CNU = 302
168600100326     C                   eval      I33NUM = 1
168700100326     C                   movel     TRUL33DS      KPJBU
168800100326     C                   call      'TRUL33R'
168900100326     C                   parm                    KPJBA
169000100326     C                   movel     KPJBU         TRUL33DS
169100100326     C                   if        O33ERR = *zeros
169200100326     C                   z-add     O33NRF        VABNSP
169300100326     C                   z-add     O33NRF        VATNSP
169400100326     C                   else
169500100326     C                   SETON                                        31
169600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
169700100326     C                             + ' ' + 'VABNSP VATNSP'
169800100326     C                   endif
169900100326     C*
170000100326     C                   ENDSR
170100010330
170200100326
170300010330
170400000613     C     *inzsr        BEGSR
170500990910     C*
170600990910     C     *entry        plist
170700990920     C                   parm                    tivlrds
170800990921     C                   parm      wrkesito      esito
170900000724     C                   parm                    prmlit
171000000710     C                   parm                    prmfir
171100010330     C*
171200010330     C* CALCOLA LA DATA CORRENTE
171300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
171400100318     C                   eval      datcor = %dec(%date() : *ISO)
171500000613     C*
171600000613     C                   ENDSR
171700000613     C***
171800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
171900100326VABRSD;VATNOT_A;VABIND;VABCAD;VABLOD;VABPRD;W_VABRMN;VABNCL;VABCAS;
172000110714VABNAS;VABPKB;W_VABNOT;VATNOT_B;
172100100318
172200100318
172300100318
