000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120604      *
000300120604      * PARTICOLARITA':
000400120711      * Il cliente trasmetterà una riga per collo e vanno consolidate le righe con lo stesso Numero
000500120711      * Fattura ( seconda colonna)
000600120711      * ATTENZIONE il contrassegno da incassare viene riportato su ogni riga ma NON è da sommare
000700120604
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller) BNDDIR('UBBNDDIR')
000900120209     F*
001000990910     Ftivin00r  uF   E             DISK    usropn
001100100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001200040119     FFIVATwwr  O    E             DISK    usropn
001300000313     D*
001400040113     D*------------
001500040113     D* SCHIEDE A PROGRAMMA
001600040113     D*------------
001700080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001800010330     D*----------------------------------------------------
001900010330     D* DICHIARAZIOINE VARIABILI DI WRK
002000010330     D*----------------------------------------------------
002100010330     D dscmz         e ds                  inz
002200010330     D psds           sds
002300010330     D  procname         *PROC
002400010330     D tivlrds       e ds                  extname(tivlr00f)
002500040128     D tisi95ds      e ds
002600100413     D fivabds       e ds                  extname(fivab00f)
002700100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002800100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D rrnum           s              6  0 INZ(*zeros)
003400040510     D parccm          s              8    INZ(*blanks)
003500040119     D parmbr          s             10    INZ(*blanks)
003600040119     D paropz          s              1    INZ(*blanks)
003700040119     D chkcall         s              1    INZ(*blanks)
003800080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003900080418     D wFlgCAS         s              1    INZ(*blanks)
004000120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004100120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004200120711     D Intestazioni    s               n   inz(*off)
004300100326     D*------------------
004400100326     D* DS REPERIMENTO NUMERATORE
004500100326     D*------------------
004600100326     D trul33ds      e ds                  inz
004700100326     D*------------------
004800100326     D* DS ARCHITETTURA
004900100326     D*------------------
005000100326     D kpjba         e ds                  inz
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
006100030715     D posDa           S              3  0 INZ(*zeros)
006200030715     D posA            S              3  0 INZ(*zeros)
006300030715     D i               s              3  0 INZ(1)
006400030715     D wGiro           s              1  0 INZ(*zeros)
006500070823     D wVATNOT_A       s                   LIKE(VATNOT)
006600060331     D wVATNOT_B       s                   LIKE(VATNOT)
006700060331     D wVATNOT_E       s                   LIKE(VATNOT)
006800110927     D wVATNOT_I       s                   LIKE(VATNOT)
006900120411     D wNOT            s             70
007000120411     D Data8A          s              8
007100120709     D wBarcode        s             11
007200120709     D wBarcodeN       s             11s 0
007300120629     D wX              s              3s 0
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
009200010330
009300000913     C                   reset                   rrnum
009400990921     C                   reset                   esito
009500990921     C                   reset                   wrkesito
009600010601     C*
009700010601     C                   exsr      opeini
009800010605     C                   exsr      rwvab
009900010601     C*
010000010601     C                   seton                                        lr
010100010601
010200010601
010300010601
010400010601     C*--------------------------------------------------------
010500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010600010601     C*--------------------------------------------------------
010700010601     C     PREELA        BEGSR
010800010601     C*
010900010601     C* SE OCCORRE SPEDIRE IN FILIALE
011000010601     C                   if        invfil <> *zeros and
011100010601     C                             flgGiro = '0'
011200010601     C*
011300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011400010601     C                   eval      flgGiro = '1'
011500010601     C*
011600010601     C                   endif
011700010601     C*
011800010601     C                   ENDSR
011900010601     C***
012000010601
012100010601
012200010601
012300010601
012400010601     C*--------------------------------------------------------
012500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012600010601     C*--------------------------------------------------------
012700010601     C     ENDELA        BEGSR
012800040119     C*
012900040119     C* Effettuo la chiamata al CLLE preposto
013000040119     C                   call(e)   'TITVVTC'
013100040510     C                   parm                    parccm
013200040119     C                   parm                    parmbr
013300040119     C                   parm      '2'           paropz
013400050201     C*
013500050201     C* Effettuo lancio TISI95 solo x chiusura
013600050201     C                   CLEAR                   TISI95DS
013700050201     C                   EVAL      I95TLA = 'C'
013800050201     C                   CALL      'TISI95R'
013900050201     C                   PARM                    TISI95DS
014000000616     C*
014100010601     C                   ENDSR
014200010601
014300010601
014400010330     C*--------------------------------------------------------
014500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014600010330     C*--------------------------------------------------------
014700010605     C     RWVAB         BEGSR
014800010330     C*
014900010330     C                   if        not %open(tivin00r)
015000010330     C                   open      tivin00r
015100010330     C                   endif
015200021025     C                   if        not %open(fivabwwr)
015300021025     C                   open      fivabwwr
015400010330     C                   endif
015500040119     C*
015600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015700040119     C                   exsr      prevat
015800040119     C*
015900040119     C                   if        chkcall = '0'
016000040119     C*
016100040119     C                   if        not %open(fivatwwr)
016200040119     C                   open      fivatwwr
016300040119     C                   endif
016400010330     C*
016500040119     C                   clear                   §CTROKVB          7 0
016600040119     C                   clear                   §CTROKVT          7 0
016700040119     C                   clear                   §CTRMO            7 0
016800040119     C                   clear                   §CTRNO            7 0
016900120410     C*
017000120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017100120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017200120410     C                   IF        Intestazioni = *off
017300120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017400120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017500120410     C                   exsr      inzvar
017600120410     C                   exsr      impvab
017700120410     C                   ENDIF
017800010330     C*
017900030822     C                   do        *HIVAL
018000010330     C*
018100030822     C                   read      tivin00r                               70
018200010618     C*
018300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018400010618     C                   if        vinflg = '1'
018500010618     C                   eval      flgOk = '1'
018600010618     C                   endif
018700040510     C*
018800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018900040510     C                   z-add     *zeros        wLenVINDTA        4 0
019000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019400040510     C                   z-add     1             wFlgVINDTA
019500040510     C                   leave
019600040510     C                   endif
019700040510     C                   enddo
019800010618     C*
019900040510     C                   if        vindta > *blanks AND
020000040510     C                             wFlgVINDTA = 1
020100010330     C                   add       1             rrnum
020200010330     C*
020300010601     C                   if        *in70 = *off and
020400010330     C                             (vinflg = *blanks
020500010330     C                              or vinflg = '0'
020600010330     C                              or vinflg = '2')
020700010330     C*
020800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020900010711     C                   if        vinflg = *blanks or vinflg = '0'
021000010711     C                   clear                   vinmsg
021100010711     C                   endif
021200010601     C*
021300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021400070925     C                   eval      vindta  = ' ' + %trim(vindta)
021500070925     C*
021600080527     C                   exsr      inzvar
021700100318     C                   exsr      normCSV
021800010605     C                   exsr      impvab
021900010601     C*
022000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022100010605     C                   eval      depfil = VABLNP
022200010601     C                   exsr      repfil
022300010601     C                   if        depfil = invfil
022400021025     C                   if        vlrpoi = 999
022500030822     C                   move(P)   invfil        VABFGS
022600021025     C                   else
022700030822     C                   move(P)   vlrpoi        VABFGS
022800021025     C                   endif
022900010601     C*
023000010601     C                   exsr      PREELA
023100010601     C*
023200010604     C* Ebbene...
023300010604     C*
023400040119     C  N31              add       1             §CTROKVB
023500040119     C   32              add       1             §CTRMO
023600040119     C   31              add       1             §CTRNO
023700080418     C*
023800030822     C                   if        wGiro = 2
023900080513     C* Scrivo sempre il VAT
024000080513     C  N31              exsr      wrivat
024100110201     C                   exsr      chkvab
024200080513     C                   endif
024300010604     C*
024400010604     C                   if        *in31 = *off and
024500010604     C                             *in32 = *off
024600010604     C                   eval      vinflg = '1'
024700010604     C                   else
024800010604     C                   eval      vinflg = '2'
024900010604     C                   endif
025000010604     C                   endif
025100010601     C*
025200010604     C                   endif
025300010604     C*
025400010330     C                   else
025500010330     C                   eval      vinflg = '1'
025600010330     C                   endif
025700010601     C*
025800010601     C  N70              update    tivin000
025900010330     C*
026000030822     C  N70              enddo
026100040119     C*
026200080527     C* Verifico dati rimasti "in cannna"...
026300080527     C                   seton                                        65
026400110201     C                   exsr      chkvab
026500080527     C*
026600040119     C                   endif                                                  (endif - chkcall)
026700010601     C*
026800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026900010601     C                   if        cntNonEl = *zeros or
027000010601     C                             flgMulti = '0'
027100010330     C* Se non ci sono record con errori ...
027200010601     C                   if        §ctrno = 0 and
027300010604     C                             §ctrmo = 0 and
027400010601     C                             flgStato <> '2'
027500010330     C* ... restituisco esito OK.
027600010330     C                   eval      wrkesito = '0'
027700010330     C                   else
027800040119     C                   if        §ctrokvb > 0
027900010330     C                   eval      wrkesito = '1'
028000010330     C                   else
028100010615     C                   if        flgOk = '0'
028200010615     C                   eval      wrkesito = '2'
028300010615     C                   else
028400010615     C                   eval      wrkesito = '6'
028500010615     C                   endif
028600010330     C                   endif
028700010330     C                   endif
028800010601     C                   else
028900010601     C                   eval      wrkesito = '9'
029000010601     C                   endif
029100010330     C*
029200010330     C                   if        %open(tivin00r)
029300010330     C                   close     tivin00r
029400010330     C                   endif
029500021025     C                   if        %open(fivabwwr)
029600021025     C                   close     fivabwwr
029700010330     C                   endif
029800040512     C                   if        %open(fivatwwr)
029900040512     C                   close     fivatwwr
030000040512     C                   endif
030100010601     C*
030200010601     C                   if        vlrpoi <> 999
030300010601     C                   eval      invfil = vlrpoi
030400010601     C                   endif
030500010330     C*
030600040119     C                   if        §ctrokvb > 0
030700010601     C                             and invfil > *zeros
030800010330     C                   exsr      invio
030900010330     C                   endif
031000010601     C*
031100010618     C                   if        flgGiro = '1'
031200010601     C                   exsr      endela
031300010618     C                   endif
031400010330     C*
031500010330     C                   ENDSR
031600010330     C***
031700080527
031800080527
031900080527
032000080527     C*----------------------------------------------------*
032100080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032200080527     C*----------------------------------------------------*
032300100413     C     CHKVAB        BEGSR
032400110201     C*
032500110201     C* Se richiesti ignorare colli / peso / volume
032600110315 xxx C***                if        *in35
032700110315 xxx C***                z-add     *zeros        VABNCL
032800110315 xxx C***                z-add     *zeros        VABPKB
032900110315 xxx C***                z-add     *zeros        VABVLB
033000110315 xxx C***                endif
033100080527     C*
033200080527     C* Scrivo il VAB solo a rottura d spedizione
033300080527     C                   if        depspe = *zeros
033400080527     C  N31              eval      fivabds_sav = fivabds
033500080527     C                   eval      depspe = vabNSP
033600080527     C                   else
033700080527     C                   if        depspe = vabNSP AND *in65 = *off
033800120703     C   51              add       VABNCL        sav_VABNCL
033900100413     C   51              add       VABPKB        sav_VABPKB
034000100413     C   51              add       VABVLB        sav_VABVLB
034100100720     C   52              z-add     VABNCL        sav_VABNCL
034200100720     C   52              z-add     VABPKB        sav_VABPKB
034300100720     C   52              z-add     VABVLB        sav_VABVLB
034400100720     C   53              add       VABNCL        sav_VABNCL
034500100720     C   53              z-add     VABPKB        sav_VABPKB
034600100720     C   53              z-add     VABVLB        sav_VABVLB
034700080527     C                   else
034800080527     C  N31              eval      fivabds_ok = fivabds_sav
034900080527     C  N31              exsr      wrivab
035000080527     C  N31              eval      fivabds_sav = fivabds
035100080527     C                   eval      depspe = vabNSP
035200080527     C                   endif
035300080527     C                   endif
035400080527     C*
035500080527     C                   ENDSR
035600040119
035700040119
035800040119
035900040119     C*----------------------------------------------------*
036000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036100040119     C*----------------------------------------------------*
036200040510     C     WRIVAB        BEGSR
036300080513     C*
036400080513     C* Gestisco l'eventuale rottura x numero spedizione
036500080527     C                   if        VABNSP <> depspe or *in65 = *on
036600070301     C                   if        wVATNOT_E <> *blanks
036700160823     C                   eval      ok_VABCTM = '7Q'
036800070301     C                   endif
036900160823     C* se la tariffa che sto per scrivere è 300, allora  il trattamento merce è 7R
037000160823     C                   if        ok_VABCTR = 300
037100160823     C                   eval      ok_VABCTM = '7R'
037200150629     C                   endif
037300100413     C                   write     fivab000
037400080513     C                   endif
037500040119     C*
037600040119     C                   ENDSR
037700040119
037800040119
037900040119
038000040119     C*----------------------------------------------------*
038100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038200040119     C*----------------------------------------------------*
038300040510     C     WRIVAT        BEGSR
038400060512     C*
038500060512     C* Inizializzo i campi chiave
038600060512     C                   eval      VATFGS = VABFGS
038700060512     C                   eval      VATAAS = VABAAS
038800060512     C                   eval      VATCCM = VABCCM
038900070823     C                   eval      VATNRS = VABNRS
039000060512     C                   eval      VATNSP = VABNSP
039100060512     C                   eval      VATLNP = VABLNP
039200040119     C*
039300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039400040510     C                   if        VATTRC = *blanks
039500040510     C                   eval      VATTRC = 'E'
039600040510     C                   endif
039700110318     C                   if        VATNOT <> *blanks
039800040510     C                   write     FIVAT000
039900040119     C                   add       1             §CTROKVT
040000110318     C                   endif
040100060331     C*
040200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040300060331     C                   if        wVATNOT_A <> *blanks
040400060331     C                   eval      VATNOT = wVATNOT_A
040500060331     C                   eval      VATTRC = 'A'
040600060331     C                   write     FIVAT000
040700060331     C                   add       1             §CTROKVT
040800060331     C                   endif
040900060331     C*
041000060331     C                   if        wVATNOT_B <> *blanks
041100060331     C                   eval      VATNOT = wVATNOT_B
041200060331     C                   eval      VATTRC = 'B'
041300060331     C                   write     FIVAT000
041400060331     C                   add       1             §CTROKVT
041500060331     C                   endif
041600060331     C*
041700060331     C                   if        wVATNOT_E <> *blanks
041800060331     C                   eval      VATTRC = 'E'
041900120629     C*
042000120711     C* NON sviluppo i "CHI SONO" perché sono indicati sul csv 1 riga x collo
042100120703     C*
042200120711     C                   EVAL      VATNOT = wVATNOT_E
042300120629     C                   write     FIVAT000
042400120629     C                   add       1             §CTROKVT
042500120604     C*
042600120604     C                   endif
042700110927     C*
042800110927     C                   if        wVATNOT_I <> *blanks
042900110927     C                   eval      VATNOT = wVATNOT_I
043000110927     C                   eval      VATTRC = 'I'
043100110927     C                   write     FIVAT000
043200110927     C                   add       1             §CTROKVT
043300110927     C                   endif
043400040119     C*
043500040119     C                   ENDSR
043600040119
043700040119
043800040119
043900040119     C*----------------------------------------------------*
044000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044100040119     C*----------------------------------------------------*
044200040119     C     PREVAT        BEGSR
044300040119     C*
044400040119     C* Compongo il nome del membro da dare al FIVATWWR
044500040119     C                   eval      parmbr = vlrhdl
044600040119     C                   movel     'M'           parmbr
044700040510     C                   eval      parccm = vlrksc
044800040119     C                   eval      paropz = '1'
044900040119     C* Effettuo la chiamata al CLLE preposto
045000040119     C                   call(e)   'TITVVTC'
045100040119     C                   parm                    parccm
045200040119     C                   parm                    parmbr
045300040119     C                   parm                    paropz
045400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045500040119     C                   if        %error
045600040119     C                   movel     '1'           chkcall
045700040119     C                   else
045800040119     C                   movel     '0'           chkcall
045900040119     C                   endif
046000040119     C*
046100040119     C                   ENDSR
046200010601
046300010601
046400010601
046500010330     C*----------------------------------------------------*
046600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046700010330     C*----------------------------------------------------*
046800040119     C     INZVAR        BEGSR
046900080527     C*
047000080527     C                   setoff                                       65
047100010330     C*
047200030822     C* Inizializzo variabili di wrk
047300020204     C                   Z-ADD     *zeros        Num5_0            5 0
047400101018     C                   MOVEL     *blanks       wFlgCAS
047500060331     C                   MOVEL     *blanks       wVATNOT_A
047600060331     C                   MOVEL     *blanks       wVATNOT_B
047700060331     C                   MOVEL     *blanks       wVATNOT_E
047800110927     C                   MOVEL     *blanks       wVATNOT_I
047900120411     C                   MOVEL     *blanks       wNOT
048000120411     C                   MOVEL     *blanks       Data8A
048100030822     C*
048200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048300030822     C                   CLEAR                   SkSplitCSV
048400030822     C                   EVAL      i = 1
048500030822     C                   EVAL      posDa = *zeros
048600030822     C                   EVAL      posA  = *zeros
048700080527     C*
048800080527     C                   CLEAR                   FIVABDS
048900080527     C                   CLEAR                   FIVAT000
049000030822     C*
049100030822     C* Reimposto i valori di default
049200030822     C                   EXSR      DEFCAM
049300030822     C*
049400010330     C                   ENDSR
049500010330     C*----------------------------------------------------*
049600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049700010330     C*----------------------------------------------------*
049800010330     C     DEFCAM        BEGSR
049900030822     C*
050000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050100030715     C* e delimitatore testo.
050200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050500100318     C*
050600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050700100318     C                   EVAL      CharSOS = CharNUM
050800030822     C*
050900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051000100720     C                   SETOFF                                       505152
051100100720     C                   SETOFF                                       53
051200100413     C                   SELECT
051300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051400100720     C                   SETON                                        50
051500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051600100413     C                   SETON                                        51
051700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051800100720     C                   SETON                                        52
051900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052000100720     C                   SETON                                        53
052100100413     C                   ENDSL
052200030822     C*
052300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052400030822     C                   EVAL      posDaDft = 1
052500030822     C                   EVAL      posADft  = 0
052600030822     C                   EVAL      wGiroDft = 0
052700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052800030822     C                             posDaDft > 0
052900030822     C*
053000030822     C* Gestisco il 1° giro
053100030822     C                   IF        wGiroDft = 0
053200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053400030822     C* Incremento il contatore dei "giri"
053500030822     C                   EVAL      wGiroDft = 1
053600030822     C                   ELSE
053700030822     C                   EVAL      posDaDft = posADft
053800030822     C                   ENDIF
053900030822     C* Eseguo lo scan x trovare la fine del campo corrente
054000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054100030822     C*
054200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054400030822     C                   IF        posDaDft > 0 AND
054500030822     C                             posADft  > 0
054600030822     C* NCL
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'NCL'
055100030822     C                   EVAL      PiStr=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   EXSR      CHKNUM
055500030822     C                   IF        PiInt=*on
055600030822     C                   Z-ADD     PiVal         VABNCL
055700030822     C                   ENDIF
055800030822     C                   ENDIF
055900030822     C* CCM
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'CCM'
056400030822     C                   EVAL      PiStr=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   EXSR      CHKNUM
056800030822     C                   IF        PiInt=*on
056900030822     C                   Z-ADD     PiVal         VABCCM
057000030822     C                   ENDIF
057100030822     C                   ENDIF
057200030822     C* LNP
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'LNP'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiInt=*on
058200030822     C                   Z-ADD     PiVal         VABLNP
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* NRS
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'NRS'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABNRS
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* CTR
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'CTR'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABCTR
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* PKB
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'PKB'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiNum=*on
062100120418     C                   Z-ADD     PiVal         VABPKB
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* VLB
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'VLB'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiNum=*on
063400030822     C                   Z-ADD     PiVal         VABVLB
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* QFT
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'QFT'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiNum=*on
064700030822     C                   Z-ADD     PiVal         VABQFT
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* CBO
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'CBO'
065500030822     C                   EVAL      VABCBO=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* TSP
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'TSP'
066400030822     C                   EVAL      VABTSP=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* VAS
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'VAS'
067300030822     C                   EVAL      VABVAS=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* VCA
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'VCA'
068200030822     C                   EVAL      VABVCA=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600070925     C* TIC
068700070925     C                   IF        %subst(
068800070925     C                             %subst(vlrppt:posDaDft+1:
068900070925     C                             posADft-posDaDft-1):1:3)
069000070925     C                             = 'TIC'
069100070925     C                   EVAL      VABTIC=%trim(%subst(
069200070925     C                             %subst(vlrppt:posDaDft+1:
069300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400070925     C                   ENDIF
069500030822     C* GCA
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'GCA'
070000030822     C                   EVAL      VABGCA=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* CTM
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'CTM'
070900030822     C                   EVAL      VABCTM=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* FFD
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'FFD'
071800030822     C                   EVAL      VABFFD=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* VAD
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'VAD'
072700030822     C                   EVAL      VABVAD=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* GMA
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'GMA'
073600030822     C                   EVAL      VABGMA=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* GGA
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'GGA'
074500030822     C                   EVAL      VABGGA=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* GVA
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'GVA'
075400030822     C                   EVAL      VABGVA=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* TC1
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'TC1'
076300030822     C                   EVAL      VABTC1=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* TC2
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'TC2'
077200030822     C                   EVAL      VABTC2=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600040714     C* VATTRC
077700040714     C                   IF        %subst(
077800040714     C                             %subst(vlrppt:posDaDft+1:
077900040714     C                             posADft-posDaDft-1):1:3)
078000040714     C                             = 'TRC'
078100040714     C                   EVAL      VATTRC=%trim(%subst(
078200040714     C                             %subst(vlrppt:posDaDft+1:
078300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400040714     C                   ENDIF
078500030822     C* ...
078600030822     C                   ENDIF
078700030822     C                   ENDDO
078800020204     C*
078900010330     C                   ENDSR
079000010607     C*----------------------------------------------------*
079100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079200010607     C*----------------------------------------------------*
079300010607     C     IMPVAB        BEGSR
079400010607     C*
079500010607     C                   SETOFF                                       3132
079600110315 xxx C***                SETON                                        35
079700030515     C*
079800010607     C                   MOVEL     datcor        VABAAS
079900010607     C                   MOVE      datcor        VABMGS
080000010607     C*
080100030715     C**********
080200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080300030715     C**********
080400030715     C                   IF        wGiro = *zeros
080500040113     C*
080600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080900080429     C                             %trim(intcol(5))<>*blanks
081000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081200080429     C                                      %trim(intcol(5))
081300070823     C     ';':CharCSV   XLATE     vindta        vindta
081400040510     C                   ENDIF
081500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081600031201     C     minu:maiu     XLATE     vindta        vindta
081700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081800030715     C                   DOW       posDa <= %len(%trim(vindta))
081900030715     C*
082000030715     C* Gestisco il 1° campo
082100030715     C                   IF        i = 1
082200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082300030715     C                   EVAL      posDa = 1
082400030715     C* Eseguo lo scan x trovare la fine del primo campo
082500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082600030715     C* A questo punto "estraggo" il campo corrente
082700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082800030715     C                                                    (posA-posDa))
082900030715     C* X i campi successivi al 1°
083000030715     C                   ELSE
083100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083200030715     C                   EVAL      posDa = posA + 1
083300030715     C* Eseguo lo scan x trovare la fine del campo corrente
083400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083500030715     C* Gestisco l'ultimo campo
083600030715     C                   IF        posA = *zeros
083700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083800030715     C* A questo punto "estraggo" il campo corrente
083900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084000030715     C                                                    (posA-posDa))
084100030715     C                   LEAVE
084200030715     C                   ELSE
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C                   ENDIF
084700030715     C                   ENDIF
084800030715     C* Incremento il contatore d campo
084900030715     C                   EVAL      i = i +1
085000030715     C                   ENDDO
085100030715     C*
085200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085300030715     C* delimitazione testo
085400030715     C                   EVAL      i = 1
085500030715     C                   DOW       i < %elem(SkSplitFLD)
085600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085800030715     C                   EVAL      i = i + 1
085900030715     C                   ENDDO
086000030715     C                   EVAL      wGiro = 1
086100030715     C                   ELSE
086200030715     C                   EVAL      wGiro = 2
086300031204     C**********
086400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086500031204     C* d separatore campo
086600031204     C**********
086700031204     C                   EVAL      vindta = %trim(vindta)
086800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086900031204     C                   EVAL      vindta = %subst(vindta:2)
087000031204     C                   ENDDO
087100031204     C*
087200031204     C                   Z-ADD     *zeros        lunghInput        4 0
087300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087700110912     C                   ENDDO
087800030715     C**********
087900030715     C* Effettuo lo split del campo dati d input
088000030715     C**********
088100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088200030715     C                   DOW       posDa <= %len(%trim(vindta))
088300030715     C*
088400030715     C* Gestisco il 1° campo
088500030715     C                   IF        i = 1
088600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088700030715     C                   EVAL      posDa = 1
088800030715     C* Eseguo lo scan x trovare la fine del primo campo
088900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089000030715     C* A questo punto "estraggo" il campo corrente
089100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089200030715     C                                                    (posA-posDa))
089300030715     C* X i campi successivi al 1°
089400030715     C                   ELSE
089500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089600030715     C                   EVAL      posDa = posA + 1
089700031204     C* Verifico che nn vi sia il campo nn valorizzato
089800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089900031204     C* Se campo nn valorizzato skippo al prossimo
090000031204     C                   EVAL      SkSplitCSV(i) = *blanks
090100031204     C                   EVAL      posA  = posA + 1
090200031204     C                   ELSE
090300030715     C* Eseguo lo scan x trovare la fine del campo corrente
090400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090500030715     C* Gestisco l'ultimo campo
090600030715     C                   IF        posA = *zeros
090700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090800030715     C* A questo punto "estraggo" il campo corrente
090900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091000030715     C                                                    (posA-posDa))
091100030715     C                   LEAVE
091200030715     C                   ELSE
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   ENDIF
091700031204     C                   ENDIF
091800030715     C                   ENDIF
091900030715     C* Incremento il contatore d campo
092000030715     C                   EVAL      i = i +1
092100030715     C                   ENDDO
092200030715     C*
092300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092400030715     C* delimitazione testo
092500030715     C                   EVAL      i = 1
092600030715     C                   DOW       i < %elem(SkSplitCSV)
092700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092900030715     C                   EVAL      i = i + 1
093000030715     C                   ENDDO
093100100720     C*
093200120711     C* NON Stacco un progressivo x numero spedizione perché il dato è nel csv
093300120711 xxx C***                   EXSR      repNSP
093400100326     C*
093500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093600030715     C                   EVAL      i = 1
093700030715     C                   DOW       i < %elem(SkSplitFLD)
093800030715     C*
093900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094100030715     C                             SkSplitCSV(i) <> *blanks
094200030721     C*** ATB
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094400030721     C                   EVAL      VABATB = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030715     C*** RSD
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095100030715     C* ==
095200030715     C                   ENDIF
095300030721     C*** RD2
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030715     C*** IND
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095900030715     C                   EVAL      VABIND = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030715     C*** LOD
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030715     C*** PRD
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030721     C*** NZD
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097200150609     C* se VABNZD contiene 'I', 'IT' o 'ITA' viene cambiato in blank
097300150609     C                   IF        VABNZD = 'I' or
097400150609     C                             VABNZD = 'IT' or
097500150609     C                             VABNZD = 'ITA'
097600150609     C                   EVAL      VABNZD = *blank
097700150609     C                   ENDIF
097800030721     C                   ENDIF
097900030715     C*** RMA
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** NOT
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** NT2
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** VCA
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** VAS
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** FFD
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030721     C*** GC1
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** GC2
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** TSP
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101400030721     C                   ENDIF
101500040510     C*** CBO
101600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101800040510     C                   ENDIF
101900030721     C*** NAS
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** TIC
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** GCA
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** XCO
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** CTM
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TCR
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** CTS
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** FTM
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** VAD
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GMA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** GGA
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GVA
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TC1
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TC2
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** SCL
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** RMO
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** NMO
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*
108800030721     C* Reperisco quindi i campi numerici...
108900040510     C*** NSP
109000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109100040510     C                   EVAL      PiStr=SkSplitCSV(i)
109200040510     C                   EXSR      CHKNUM
109300040510     C                   IF        PiInt=*on
109400040510     C                   Z-ADD     PiVal         VABNSP
109500040510     C                   ELSE
109600040510     C                   SETON                                        31
109700040510     C                   EVAL      VABNSP = *zeros
109800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109900040510     C                             + ' ' + 'VABNSP'
110000040510     C                   ENDIF
110100040510     C                   ENDIF
110200071106     C*** RMN
110300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110400071106     C                   EVAL      PiStr=SkSplitCSV(i)
110500071106     C                   EXSR      CHKNUM
110600071106     C                   IF        PiInt=*on
110700071106     C                   Z-ADD     PiVal         VABRMN
110800071106     C                   ELSE
110900071106     C                   SETON                                        32
111000071106     C                   EVAL      VABRMN = *zeros
111100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111200071106     C                             + ' ' + 'VABRMN'
111300071106     C                   ENDIF
111400071106     C                   ENDIF
111500030721     C*** CAD
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111700160215     C                   EVAL      VABCAD = SkSplitCSV(i)
111800160215     C***                EVAL      PiStr=SkSplitCSV(i)
111900160215     C***                EXSR      CHKNUM
112000160215     C***                IF        PiInt=*on
112100160215     C***                Z-ADD     PiVal         Num5_0
112200160215     C***                MOVEL(p)  Num5_0        VABCAD
112300160215     C***                ELSE
112400160215     C***                SETON                                        32
112500160215     C***                EVAL      VABCAD = *zeros
112600160215     C***                EVAL      vinmsg = %trimr(vinmsg)
112700160215     C***                          + ' ' + 'VABCAD'
112800160215     C***                ENDIF
112900030721     C                   ENDIF
113000030721     C*** NCL
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiInt=*on
113500030721     C                   Z-ADD     PiVal         VABNCL
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      VABNCL = *zeros
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000040122     C                             + ' ' + 'VABNCL'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** PKB
114400120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
114500120418     C* per cui si arrotonda il valore passato agli etti
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiNum=*on
115000120418     C                   Z-ADD(H)  PiVal         VABPKB
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABPKB = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABPKB'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** CAS
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116000110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116100111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
116200120713     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
116300120713     C                             %trim(SkSplitCSV(i)) <> '0,00'
116400120605     C                   EVAL      VABVCA = 'EUR'
116500101018     C                   EVAL      wFlgCAS = '1'
116600101018     C                   ENDIF
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiNum=*on
117000030721     C                   Z-ADD     PiVal         VABCAS
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      VABCAS = *zeros
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500030721     C                             + ' ' + 'VABCAS'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** CCM
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiInt=*on      AND
118300030721     C                             PiVal<=9999999 AND
118400030721     C                             PiVal>*zeros
118500030721     C                   Z-ADD     PiVal         VABCCM
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      VABCCM = *zeros
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000040510     C                             + ' ' + 'VABCCM'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** LNP
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on    AND
119800030721     C                             PiVal<=999   AND
119900030721     C                             PiVal>*zeros
120000030721     C                   Z-ADD     PiVal         VABLNP
120100030721     C                   ELSE
120200040715     C                   SETON                                        32
120300030721     C                   EVAL      VABLNP = *zeros
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500040510     C                             + ' ' + 'VABLNP'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** NRS
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiInt=*on AND
121300030721     C                             PiVal<=99
121400030721     C                   Z-ADD     PiVal         VABNRS
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABNRS'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** LNA
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiInt=*on    AND
122600030721     C                             PiVal<=999   AND
122700030721     C                             PiVal>*zeros
122800030721     C                   Z-ADD     PiVal         VABLNA
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      VABLNA = *zeros
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300030721     C                             + ' ' + 'VABLNA'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** CTR
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiInt=*on  AND
124100030721     C                             PiVal<=999
124200030721     C                   Z-ADD     PiVal         VABCTR
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABCTR'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** IAS
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiNum=*on
125400030721     C                   Z-ADD     PiVal         VABIAS
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABIAS'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** VLB
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiNum=*on
126600030721     C                   Z-ADD     PiVal         VABVLB
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABVLB'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** QFT
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiNum=*on
127800030721     C                   Z-ADD     PiVal         VABQFT
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABQFT'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** NCD
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on      AND
129000030721     C                             PiVal<=9999999
129100030721     C                   Z-ADD     PiVal         VABNCD
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABNCD'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** NCA
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiInt=*on      AND
130300030721     C                             PiVal<=9999999
130400030721     C                   Z-ADD     PiVal         VABNCA
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABNCA'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** ZNC
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiInt=*on AND
131600030721     C                             PiVal<=99
131700030721     C                   Z-ADD     PiVal         VABZNC
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABZNC'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** DCR
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on       AND
132900030721     C                             PiVal<=99999999
133000030721     C                   Z-ADD     PiVal         VABDCR
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABDCR'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030721     C*** HCR
133800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on   AND
134200030721     C                             PiVal<=9999
134300030721     C                   Z-ADD     PiVal         VABHCR
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134700030721     C                             + ' ' + 'VABHCR'
134800030721     C                   ENDIF
134900030721     C                   ENDIF
135000030721     C*** VMD
135100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135200030721     C                   EVAL      PiStr=SkSplitCSV(i)
135300030721     C                   EXSR      CHKNUM
135400030721     C                   IF        PiNum=*on
135500030721     C                   Z-ADD     PiVal         VABVMD
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABVMD'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** ANT
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiInt=*on        AND
136700030721     C                             PiVal<=999999999
136800030721     C                   Z-ADD     PiVal         VABANT
136900030721     C                   ELSE
137000030721     C                   SETON                                        32
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABANT'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500030721     C*** CMO
137600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiInt=*on
138000030721     C                   Z-ADD     PiVal         Num5_0
138100030721     C                   MOVEL(p)  Num5_0        VABCMO
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400030721     C                   EVAL      VABCMO = *zeros
138500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138600030721     C                             + ' ' + 'VABCMO'
138700030721     C                   ENDIF
138800030721     C                   ENDIF
138900040510     C*
139000040510     C* Reperisco i campi del FIVAT
139100040510     C*** VATNOT
139200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139400040510     C                   ENDIF
139500040510     C*** VATTRC
139600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139800040510     C                   ENDIF
139900060331     C*** VATNOT_A
140000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140200060331     C                   ENDIF
140300060331     C*** VATNOT_B
140400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140600060331     C                   ENDIF
140700060331     C*** VATNOT_E
140800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141000060331     C                   ENDIF
141100110927     C*** VATNOT_I
141200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
141400110927     C                   ENDIF
141500070823     C*
141600120713 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
141700120713
141800120711     C*** W_VABNOT può contenere sia VABNOT che VABNT2
141900120711     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
142000120711     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
142100120711     C                   ENDIF
142200120717     C*** NSP / RMN / RMA
142300120713     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
142400120713     C                   EVAL      PiStr=SkSplitCSV(i)
142500120713     C                   EXSR      CHKNUM
142600120713     C                   IF        PiInt=*on
142700120713     C                   Z-ADD     PiVal         VABNSP
142800120713     C                   Z-ADD     PiVal         VABRMN
142900120717     C                   MOVEL     datcor        VABRMA
143000120717     C                   EVAL      %subst(VABRMA:5:5) = %subst(
143100120717     C                              %editc(VABRMN:'X') : 11:5)
143200120713     C                   ELSE
143300120713     C                   SETON                                        31
143400120713     C                   EVAL      VABNSP = *zeros
143500120713     C                   EVAL      VABRMN = 1
143600120713     C                   EVAL      vinmsg = %trimr(vinmsg)
143700120717     C                             + ' ' + 'VABNSP VABRMN VABRMA'
143800120713     C                   ENDIF
143900120713     C                   ENDIF
144000120301
144100070925     C*
144200071029     C***  <===  -----------------------------------------
144300040510     C*
144400030715     C                   ENDIF
144500030715     C                   EVAL      i = i + 1
144600030715     C                   ENDDO
144700070925     C*
144800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144900120411     C*
145000120711     C*** W_VABNOT può contenere sia VABNOT che VABNT2
145100120711     C                   EVAL      VABNOT = %subst(wNOT:1:35)
145200120711     C                   EVAL      VABNT2 = %subst(wNOT:36:35)
145300070925     C*
145400071029     C***  <===  -----------------------------------------
145500070925     C*
145600070925     C* Considerazioni finali su CBO/CAS
145700100318     C                   IF        wFlgCAS = '1'    OR
145800100318     C                             VABCAS  > *zeros
145900070925     C                   IF        VABCBO = '1'
146000070925     C                   EVAL      VABCBO = '4'
146100070925     C                   ENDIF
146200070925     C                   IF        VABCBO = '2'
146300070925     C                   EVAL      VABCBO = '6'
146400070925     C                   ENDIF
146500070925     C                   ENDIF
146600070925     C***  <===  ----------------------------
146700040510     C*
146800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146900040510     C                   IF        VABNSP = *zeros
147000040510     C                   SETON                                        31
147100040510     C                   ENDIF
147200040715     C*
147300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147400040715     C                   IF        VABLNP = *zeros
147500040715     C                   SETON                                        31
147600040715     C                   ENDIF
147700040510     C*
147800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147900120411     C                   IF        VABRMN = *zeros
148000120411     C                   Z-ADD     VABNSP        VABRMN
148100120411     C                   ENDIF
148200120328     C* IN QUESTO CASO LO FORZO = 1
148300120411     C***                IF        VABRMN = *zeros
148400120411     C***                Z-ADD     1             VABRMN
148500120411     C***                ENDIF
148600040510     C*
148700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148800040510     C                   IF        VABRMA = *blanks
148900040714     C                   MOVEL     VABRMN        VABRMA
149000040510     C                   ENDIF
149100040128     C*
149200040510     C* Se provincia nn valorizzata la reperisco
149300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149400110912     C                   IF        VABPRD =  *blanks AND
149500110912     C                             VABNZD =  *blanks AND
149600110912     C                             VABCAD <> *blanks AND
149700110912     C                             VABLOD <> *blanks
149800040128     C                   CLEAR                   TISI95DS
149900040128     C                   EVAL      I95TCN = '3'
150000040128     C                   Z-ADD     datcor        I95DAT
150100040128     C                   EVAL      I95NAR = VABNZD
150200040128     C                   EVAL      I95CAP = VABCAD
150300040128     C                   EVAL      I95LOC = VABLOD
150400040128     C                   CALL      'TISI95R'
150500040128     C                   PARM                    TISI95DS
150600040128     C                   EVAL      VABPRD = O95PRV
150700040510     C                   ENDIF
150800020204     C*
150900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151000020204     C                   EXSR      CHKIMPDIV
151100030715     C                   ENDIF
151200020204     C*
151300010607     C                   ENDSR
151400010607     C*----------------------------------------------------*
151500010601
151600020204
151700020204     C*----------------------------------------------------*
151800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
151900020204     C*----------------------------------------------------*
152000020204     C     CHKIMPDIV     BEGSR
152100020204     C*
152200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152300020204     C                   Z-ADD     *zeros        wrkDec            9 9
152400020204     C*
152500020204     C* Come prima cosa effettuo considerazioni sulla divisa
152600020204     C                   IF        vabIAS > *zeros
152700020204     C                   IF        vabVAS <> 'EUR'
152800020204     C                   EVAL      vabVAS =  'ITL'
152900020204     C                   ENDIF
153000020204     C                   ENDIF
153100020204     C*
153200020204     C                   IF        vabCAS > *zeros
153300020204     C                   IF        vabVCA <> 'EUR'
153400020204     C                   EVAL      vabVCA =  'ITL'
153500020204     C                   ENDIF
153600020204     C                   ENDIF
153700020204     C*
153800020204     C                   IF        vabVMD > *zeros
153900020204     C                   IF        vabVAD <> 'EUR'
154000020204     C                   EVAL      vabVAD =  'ITL'
154100020204     C                   ENDIF
154200020204     C                   ENDIF
154300020204     C*
154400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154500020204     C                   Z-ADD     vabIAS        wrkDec
154600020204     C                   IF        wrkDec > *zeros
154700020204     C                   IF        vabVAS = 'ITL'
154800020204     C                   EVAL      vabIAS = *zeros
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155300020204     C                   Z-ADD     vabCAS        wrkDec
155400020204     C                   IF        wrkDec > *zeros
155500020204     C                   IF        vabVCA = 'ITL'
155600020204     C                   EVAL      vabCAS = *zeros
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156100020204     C                   Z-ADD     vabVMD        wrkDec
156200020204     C                   IF        wrkDec > *zeros
156300020204     C                   IF        vabVAD = 'ITL'
156400020204     C                   EVAL      vabVMD = *zeros
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C                   ENDSR
156900020204     C***
157000010330
157100100318
157200100318
157300100318     C*----------------------------------------------------*
157400100318     C*  CONTROLLO NUMERICITA' CAMPI
157500100318     C*----------------------------------------------------*
157600100318     C     CHKNUM        BEGSR
157700100318     C*
157800100318     C                   IF        PiDecChr = *blanks
157900100318     C                   EVAL      PiDecChr = CharNUM
158000100318     C                   ENDIF
158100100318     C*
158200100318     C                   callp     UBISNUM_Check(PiStr
158300100318     C                                          :PiDecChr
158400100318     C                                          :PiVal
158500100318     C                                          :PiNum
158600100318     C                                          :PiInt)
158700100318     C*
158800100318     C                   ENDSR
158900100318     C***
159000010601
159100010601
159200010601
159300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159400010601     C     repfil        BEGSR
159500010601     C*
159600010601     C                   if        invfil = *zeros and
159700010601     C                             depfil > *zeros and
159800010629     C                             (vinflg = *blanks or
159900010629     C                              vinflg = *zeros)
160000010601     C
160100010601     C                   eval      invfil = depfil
160200010601     C                   endif
160300010601     C*
160400010601     C                   if        depfil <> invfil and
160500010601     C                             invfil > *zeros
160600010601     C                   eval      flgMulti = '1'
160700010601     C                   if        vinflg = *blanks
160800010601     C                   add       1             cntNonEl
160900010601     C                   endif
161000010601     C                   endif
161100010601     C*
161200010601     C                   if        vinflg = '2'
161300010601     C                   eval      flgStato = '2'
161400010601     C                   endif
161500010601     C*
161600010601     C                   ENDSR
161700010601     C***
161800010601
161900010601
162000010601
162100010330
162200040119      /TITLE Invio dei dati al punto operativo.
162300040119     C     invio         BEGSR
162400040119     C*
162500040119     C* 1° invio FIVAT
162600040119     C                   reset                   dscmz
162700040119     C                   move      vlrpoi        cmzdst
162800040119     C                   eval      cmzfld = 'FIVATWWR'
162900040119     C                   eval      cmzmbd = vlrhdl
163000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163100040119     C***                if        prmfir = *blanks
163200040119     C                   eval      cmzfla = 'FIVAT00F'
163300040119     C                   eval      cmzmba = 'FIVAT00F'
163400040119     C***                else
163500040119     C***                eval      cmzfla = prmfir
163600040119     C***                eval      cmzmba = prmfir
163700040119     C***                endif
163800040119     C                   eval      cmznrr = *zeros
163900040119     C                   move      §ctrokvt      cmznrr
164000040119     C                   eval      cmzlba = vlrfl1
164100040119     C                   call(e)   'TIS711C'
164200040119     C                   parm                    dscmz
164300040119     C                   parm      *blanks       esito
164400040119     C                   if        %error
164500040119     C                             or cmzerr = '1'
164600040119     C                             or esito  = '1'
164700040119     C                   eval      wrkesito = '3'
164800040119     C                   else
164900040119     C*
165000040119     C* 2° invio FIVAB
165100040119     C                   reset                   dscmz
165200040119     C                   move      vlrpoi        cmzdst
165300040119     C                   eval      cmzfld = vlrfou
165400040119     C                   eval      cmzmbd = vlrhdl
165500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165600040119     C***                if        prmfir = *blanks
165700040119     C                   eval      cmzfla = 'FIVAB00F'
165800040119     C                   eval      cmzmba = 'FIVAB00F'
165900040119     C***                else
166000040119     C***                eval      cmzfla = prmfir
166100040119     C***                eval      cmzmba = prmfir
166200040119     C***                endif
166300040119     C                   eval      cmznrr = *zeros
166400040119     C                   move      §ctrokvb      cmznrr
166500040119     C                   eval      cmzlba = vlrfl1
166600040119     C                   call(e)   'TIS711C'
166700040119     C                   parm                    dscmz
166800040119     C                   parm      *blanks       esito
166900040119     C                   if        %error
167000040119     C                             or cmzerr = '1'
167100040119     C                             or esito  = '1'
167200040119     C                   eval      wrkesito = '3'
167300040119     C                   endif
167400040119     C                   endif
167500040119     C*
167600040119     C                   ENDSR
167700040119     C***
167800010601
167900010601
168000010601
168100010601
168200010601      /TITLE Invio dei dati al punto operativo.
168300010601     C     opeini        BEGSR
168400010601     C*
168500010601     C* Inizializzo flag e contatori operativi
168600010601     C                   movel     '0'           flgGiro           1
168700010601     C                   movel     '0'           flgMulti          1
168800010601     C                   movel     '1'           flgStato          1
168900010615     C                   movel     '0'           flgOk             1
169000010601     C                   z-add     *zeros        cntNonEl         10 0
169100010601     C                   z-add     *zeros        depfil            3 0
169200010601     C                   z-add     *zeros        invfil            3 0
169300010601     C*
169400010601     C                   ENDSR
169500010601     C***
169600070326
169700070326
169800070326
169900070326
170000070326     C     *pssr         BEGSR
170100070329     C*
170200070329     C                   if        %open(tivin00r)
170300070329     C                   close     tivin00r
170400070329     C                   endif
170500070329     C                   if        %open(fivabwwr)
170600070329     C                   close     fivabwwr
170700070329     C                   endif
170800070329     C                   if        %open(fivatwwr)
170900070329     C                   close     fivatwwr
171000070329     C                   endif
171100070326     C*
171200070326     C* Effettuo la chiamata al CLLE preposto
171300070326     C                   call(e)   'TITVVTC'
171400070326     C                   parm                    parccm
171500070326     C                   parm                    parmbr
171600070326     C                   parm      '2'           paropz
171700070326     C*
171800070326     C                   eval      wrkesito = '2'
171900070404     C*
172000070404     C                   seton                                        LR
172100070326     C*
172200070326     C                   ENDSR     '*CANCL'
172300070326     C***
172400100318
172500100318
172600100318
172700100318     C     normCSV       BEGSR
172800100318     C*
172900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173000100318     C* separatori campi
173100100318     C                   if        CharTXT <> *blanks
173200100318     C                   z-add     *zeros        wLenVINDTA        4 0
173300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173400100318     C                   setoff                                       80
173500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173800100318     C                   if        not *in80
173900100318     C                   seton                                        80
174000100318     C                   else
174100100318     C                   setoff                                       80
174200100318     C                   endif
174300100318     C                   endif
174400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174500100318     C                             and *in80 = *on
174600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174700100318     C                   endif
174800100318     C                   enddo
174900100318     C                   endif
175000100318     C*
175100100318     C                   ENDSR
175200100318     C***
175300070326
175400100326
175500100326
175600100326     C*----------------------------------------------------*
175700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175800100326     C*----------------------------------------------------*
175900100326     C     REPNSP        BEGSR
176000100326     C*
176100100326     C* NSP => Stacco un numeratore da AZNUM
176200100326     C                   clear                   TRUL33DS
176300100326     C                   eval      I33OPE = *zeros
176400100326     C                   eval      I33CNU = 302
176500100326     C                   eval      I33NUM = 1
176600100326     C                   movel     TRUL33DS      KPJBU
176700100326     C                   call      'TRUL33R'
176800100326     C                   parm                    KPJBA
176900100326     C                   movel     KPJBU         TRUL33DS
177000100326     C                   if        O33ERR = *zeros
177100100326     C                   z-add     O33NRF        VABNSP
177200100326     C                   z-add     O33NRF        VATNSP
177300100326     C                   else
177400100326     C                   SETON                                        31
177500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177600100326     C                             + ' ' + 'VABNSP VATNSP'
177700100326     C                   endif
177800100326     C*
177900100326     C                   ENDSR
178000010330
178100100326
178200010330
178300000613     C     *inzsr        BEGSR
178400990910     C*
178500990910     C     *entry        plist
178600990920     C                   parm                    tivlrds
178700990921     C                   parm      wrkesito      esito
178800000724     C                   parm                    prmlit
178900000710     C                   parm                    prmfir
179000010330     C*
179100010330     C* CALCOLA LA DATA CORRENTE
179200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179300100318     C                   eval      datcor = %dec(%date() : *ISO)
179400000613     C*
179500000613     C                   ENDSR
179600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179700120713VATNOT_E;W_VABNSP;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_B;VABCAS;VABPKB;
179800150609W_VABNOT;VATNOT_I;VABNZD;VABCTR;
179900120604
180000120604
180100120604
