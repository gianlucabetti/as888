000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209     F*
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
003400120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
003500120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
003600120410     D Intestazioni    s               n   inz(*off)
003700100326     D*------------------
003800100326     D* DS REPERIMENTO NUMERATORE
003900100326     D*------------------
004000100326     D trul33ds      e ds                  inz
004100100326     D*------------------
004200100326     D* DS ARCHITETTURA
004300100326     D*------------------
004400100326     D kpjba         e ds                  inz
004500100326
004600030715     D*------------------
004700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004800030715     D*------------------
004900030715     D SkSplitFLD      S             10    DIM(100)
005000030715     D SkSplitCSV      S            256    DIM(100)
005100030715     D CharCSV         S              1
005200030715     D CharTXT         S              1
005300030715     D CharNUM         S              1
005400100318     D CharSOS         S              1
005500030715     D posDa           S              3  0 INZ(*zeros)
005600030715     D posA            S              3  0 INZ(*zeros)
005700030715     D i               s              3  0 INZ(1)
005800030715     D wGiro           s              1  0 INZ(*zeros)
005900070823     D wVATNOT_A       s                   LIKE(VATNOT)
006000060331     D wVATNOT_B       s                   LIKE(VATNOT)
006100060331     D wVATNOT_E       s                   LIKE(VATNOT)
006200110927     D wVATNOT_I       s                   LIKE(VATNOT)
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
007500100318
007600100318     D*------------------
007700100318     D* LINKING A DEFINIZIONI ESTERNE
007800100318     D*------------------
007900100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008000100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008100100318
008200010330
008300010330
008400010330
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800010601     C*
008900010601     C                   exsr      opeini
009000010605     C                   exsr      rwvab
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601     C*--------------------------------------------------------
009700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009800010601     C*--------------------------------------------------------
009900010601     C     PREELA        BEGSR
010000010601     C*
010100010601     C* SE OCCORRE SPEDIRE IN FILIALE
010200010601     C                   if        invfil <> *zeros and
010300010601     C                             flgGiro = '0'
010400010601     C*
010500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010600010601     C                   eval      flgGiro = '1'
010700010601     C*
010800010601     C                   endif
010900010601     C*
011000010601     C                   ENDSR
011100010601     C***
011200010601
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011800010601     C*--------------------------------------------------------
011900010601     C     ENDELA        BEGSR
012000040119     C*
012100040119     C* Effettuo la chiamata al CLLE preposto
012200040119     C                   call(e)   'TITVVTC'
012300040510     C                   parm                    parccm
012400040119     C                   parm                    parmbr
012500040119     C                   parm      '2'           paropz
012600050201     C*
012700050201     C* Effettuo lancio TISI95 solo x chiusura
012800050201     C                   CLEAR                   TISI95DS
012900050201     C                   EVAL      I95TLA = 'C'
013000050201     C                   CALL      'TISI95R'
013100050201     C                   PARM                    TISI95DS
013200000616     C*
013300010601     C                   ENDSR
013400010601
013500010601
013600010330     C*--------------------------------------------------------
013700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
013800010330     C*--------------------------------------------------------
013900010605     C     RWVAB         BEGSR
014000010330     C*
014100010330     C                   if        not %open(tivin00r)
014200010330     C                   open      tivin00r
014300010330     C                   endif
014400021025     C                   if        not %open(fivabwwr)
014500021025     C                   open      fivabwwr
014600010330     C                   endif
014700040119     C*
014800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014900040119     C                   exsr      prevat
015000040119     C*
015100040119     C                   if        chkcall = '0'
015200040119     C*
015300040119     C                   if        not %open(fivatwwr)
015400040119     C                   open      fivatwwr
015500040119     C                   endif
015600010330     C*
015700040119     C                   clear                   §CTROKVB          7 0
015800040119     C                   clear                   §CTROKVT          7 0
015900040119     C                   clear                   §CTRMO            7 0
016000040119     C                   clear                   §CTRNO            7 0
016100120410     C*
016200120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
016300120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
016400120410     C                   IF        Intestazioni = *off
016500120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016600120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016700120410     C                   exsr      inzvar
016800120410     C                   exsr      impvab
016900120410     C                   ENDIF
017000010330     C*
017100030822     C                   do        *HIVAL
017200010330     C*
017300030822     C                   read      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900040510     C*
018000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018100040510     C                   z-add     *zeros        wLenVINDTA        4 0
018200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018600040510     C                   z-add     1             wFlgVINDTA
018700040510     C                   leave
018800040510     C                   endif
018900040510     C                   enddo
019000010618     C*
019100040510     C                   if        vindta > *blanks AND
019200040510     C                             wFlgVINDTA = 1
019300010330     C                   add       1             rrnum
019400010330     C*
019500010601     C                   if        *in70 = *off and
019600010330     C                             (vinflg = *blanks
019700010330     C                              or vinflg = '0'
019800010330     C                              or vinflg = '2')
019900010330     C*
020000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020100010711     C                   if        vinflg = *blanks or vinflg = '0'
020200010711     C                   clear                   vinmsg
020300010711     C                   endif
020400010601     C*
020500070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020600070925     C                   eval      vindta  = ' ' + %trim(vindta)
020700070925     C*
020800080527     C                   exsr      inzvar
020900100318     C                   exsr      normCSV
021000010605     C                   exsr      impvab
021100010601     C*
021200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021300010605     C                   eval      depfil = VABLNP
021400010601     C                   exsr      repfil
021500010601     C                   if        depfil = invfil
021600021025     C                   if        vlrpoi = 999
021700030822     C                   move(P)   invfil        VABFGS
021800021025     C                   else
021900030822     C                   move(P)   vlrpoi        VABFGS
022000021025     C                   endif
022100010601     C*
022200010601     C                   exsr      PREELA
022300010601     C*
022400010604     C* Ebbene...
022500010604     C*
022600040119     C  N31              add       1             §CTROKVB
022700040119     C   32              add       1             §CTRMO
022800040119     C   31              add       1             §CTRNO
022900080418     C*
023000030822     C                   if        wGiro = 2
023100080513     C* Scrivo sempre il VAT
023200080513     C  N31              exsr      wrivat
023300110201     C                   exsr      chkvab
023400080513     C                   endif
023500010604     C*
023600010604     C                   if        *in31 = *off and
023700010604     C                             *in32 = *off
023800010604     C                   eval      vinflg = '1'
023900010604     C                   else
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010604     C                   endif
024300010601     C*
024400010604     C                   endif
024500010604     C*
024600010330     C                   else
024700010330     C                   eval      vinflg = '1'
024800010330     C                   endif
024900010601     C*
025000010601     C  N70              update    tivin000
025100010330     C*
025200030822     C  N70              enddo
025300040119     C*
025400080527     C* Verifico dati rimasti "in cannna"...
025500080527     C                   seton                                        65
025600110201     C                   exsr      chkvab
025700080527     C*
025800040119     C                   endif                                                  (endif - chkcall)
025900010601     C*
026000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026100010601     C                   if        cntNonEl = *zeros or
026200010601     C                             flgMulti = '0'
026300010330     C* Se non ci sono record con errori ...
026400010601     C                   if        §ctrno = 0 and
026500010604     C                             §ctrmo = 0 and
026600010601     C                             flgStato <> '2'
026700010330     C* ... restituisco esito OK.
026800010330     C                   eval      wrkesito = '0'
026900010330     C                   else
027000040119     C                   if        §ctrokvb > 0
027100010330     C                   eval      wrkesito = '1'
027200010330     C                   else
027300010615     C                   if        flgOk = '0'
027400010615     C                   eval      wrkesito = '2'
027500010615     C                   else
027600010615     C                   eval      wrkesito = '6'
027700010615     C                   endif
027800010330     C                   endif
027900010330     C                   endif
028000010601     C                   else
028100010601     C                   eval      wrkesito = '9'
028200010601     C                   endif
028300010330     C*
028400010330     C                   if        %open(tivin00r)
028500010330     C                   close     tivin00r
028600010330     C                   endif
028700021025     C                   if        %open(fivabwwr)
028800021025     C                   close     fivabwwr
028900010330     C                   endif
029000040512     C                   if        %open(fivatwwr)
029100040512     C                   close     fivatwwr
029200040512     C                   endif
029300010601     C*
029400010601     C                   if        vlrpoi <> 999
029500010601     C                   eval      invfil = vlrpoi
029600010601     C                   endif
029700010330     C*
029800040119     C                   if        §ctrokvb > 0
029900010601     C                             and invfil > *zeros
030000010330     C                   exsr      invio
030100010330     C                   endif
030200010601     C*
030300010618     C                   if        flgGiro = '1'
030400010601     C                   exsr      endela
030500010618     C                   endif
030600010330     C*
030700010330     C                   ENDSR
030800010330     C***
030900080527
031000080527
031100080527
031200080527     C*----------------------------------------------------*
031300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031400080527     C*----------------------------------------------------*
031500100413     C     CHKVAB        BEGSR
031600110201     C*
031700110201     C* Se richiesti ignorare colli / peso / volume
031800110315 xxx C***                if        *in35
031900110315 xxx C***                z-add     *zeros        VABNCL
032000110315 xxx C***                z-add     *zeros        VABPKB
032100110315 xxx C***                z-add     *zeros        VABVLB
032200110315 xxx C***                endif
032300080527     C*
032400080527     C* Scrivo il VAB solo a rottura d spedizione
032500080527     C                   if        depspe = *zeros
032600080527     C  N31              eval      fivabds_sav = fivabds
032700080527     C                   eval      depspe = vabNSP
032800080527     C                   else
032900080527     C                   if        depspe = vabNSP AND *in65 = *off
033000100413     C   51              add       VABNCL        sav_VABNCL
033100100413     C   51              add       VABPKB        sav_VABPKB
033200100413     C   51              add       VABVLB        sav_VABVLB
033300100720     C   52              z-add     VABNCL        sav_VABNCL
033400100720     C   52              z-add     VABPKB        sav_VABPKB
033500100720     C   52              z-add     VABVLB        sav_VABVLB
033600100720     C   53              add       VABNCL        sav_VABNCL
033700100720     C   53              z-add     VABPKB        sav_VABPKB
033800100720     C   53              z-add     VABVLB        sav_VABVLB
033900080527     C                   else
034000080527     C  N31              eval      fivabds_ok = fivabds_sav
034100080527     C  N31              exsr      wrivab
034200080527     C  N31              eval      fivabds_sav = fivabds
034300080527     C                   eval      depspe = vabNSP
034400080527     C                   endif
034500080527     C                   endif
034600080527     C*
034700080527     C                   ENDSR
034800040119
034900040119
035000040119
035100040119     C*----------------------------------------------------*
035200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035300040119     C*----------------------------------------------------*
035400040510     C     WRIVAB        BEGSR
035500080513     C*
035600080513     C* Gestisco l'eventuale rottura x numero spedizione
035700080527     C                   if        VABNSP <> depspe or *in65 = *on
035800070301     C                   if        wVATNOT_E <> *blanks
035900070301     C                   eval      VABCTM = '7Q'
036000070301     C                   endif
036100100413     C                   write     fivab000
036200080513     C                   endif
036300040119     C*
036400040119     C                   ENDSR
036500040119
036600040119
036700040119
036800040119     C*----------------------------------------------------*
036900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037000040119     C*----------------------------------------------------*
037100040510     C     WRIVAT        BEGSR
037200060512     C*
037300060512     C* Inizializzo i campi chiave
037400060512     C                   eval      VATFGS = VABFGS
037500060512     C                   eval      VATAAS = VABAAS
037600060512     C                   eval      VATCCM = VABCCM
037700070823     C                   eval      VATNRS = VABNRS
037800060512     C                   eval      VATNSP = VABNSP
037900060512     C                   eval      VATLNP = VABLNP
038000040119     C*
038100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038200040510     C                   if        VATTRC = *blanks
038300040510     C                   eval      VATTRC = 'E'
038400040510     C                   endif
038500110318     C                   if        VATNOT <> *blanks
038600040510     C                   write     FIVAT000
038700040119     C                   add       1             §CTROKVT
038800110318     C                   endif
038900060331     C*
039000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039100060331     C                   if        wVATNOT_A <> *blanks
039200060331     C                   eval      VATNOT = wVATNOT_A
039300060331     C                   eval      VATTRC = 'A'
039400060331     C                   write     FIVAT000
039500060331     C                   add       1             §CTROKVT
039600060331     C                   endif
039700060331     C*
039800060331     C                   if        wVATNOT_B <> *blanks
039900060331     C                   eval      VATNOT = wVATNOT_B
040000060331     C                   eval      VATTRC = 'B'
040100060331     C                   write     FIVAT000
040200060331     C                   add       1             §CTROKVT
040300060331     C                   endif
040400060331     C*
040500120320     C*
040600120410     C* i rcd di FIVAT con tipo E sono 1 in quanto è una spedizione monocollo
040700060331     C                   if        wVATNOT_E <> *blanks
040800110201     C                   eval      VATNOT = wVATNOT_E
040900060331     C                   eval      VATTRC = 'E'
041000120410     C***
041100120410     C**Sviluppati i "CHI SONO" in relazione al numero colli
041200120410     C***
041300120410     C***  1             DO        VABNCL        wNumCollo         3 0
041400120410     C*** da analisi del POC il progressivo colli è max 3 char e non contiene gli zeri non
041500120410     C*** significativi ma neanche i blank a sx
041600120410     C***                EVAL      VATNOT = %trimr(wVATNOT_E) +
041700120410     C***                             %trim(%char(wNumCollo))
041800120410     C                   write     FIVAT000
041900120410     C                   add       1             §CTROKVT
042000120410     C***                ENDDO
042100120320     C*
042200120320     C                   endif
042300120320     C*
042400110927     C*
042500110927     C                   if        wVATNOT_I <> *blanks
042600110927     C                   eval      VATNOT = wVATNOT_I
042700110927     C                   eval      VATTRC = 'I'
042800110927     C                   write     FIVAT000
042900110927     C                   add       1             §CTROKVT
043000110927     C                   endif
043100040119     C*
043200040119     C                   ENDSR
043300040119
043400040119
043500040119
043600040119     C*----------------------------------------------------*
043700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043800040119     C*----------------------------------------------------*
043900040119     C     PREVAT        BEGSR
044000040119     C*
044100040119     C* Compongo il nome del membro da dare al FIVATWWR
044200040119     C                   eval      parmbr = vlrhdl
044300040119     C                   movel     'M'           parmbr
044400040510     C                   eval      parccm = vlrksc
044500040119     C                   eval      paropz = '1'
044600040119     C* Effettuo la chiamata al CLLE preposto
044700040119     C                   call(e)   'TITVVTC'
044800040119     C                   parm                    parccm
044900040119     C                   parm                    parmbr
045000040119     C                   parm                    paropz
045100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045200040119     C                   if        %error
045300040119     C                   movel     '1'           chkcall
045400040119     C                   else
045500040119     C                   movel     '0'           chkcall
045600040119     C                   endif
045700040119     C*
045800040119     C                   ENDSR
045900010601
046000010601
046100010601
046200010330     C*----------------------------------------------------*
046300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046400010330     C*----------------------------------------------------*
046500040119     C     INZVAR        BEGSR
046600080527     C*
046700080527     C                   setoff                                       65
046800010330     C*
046900030822     C* Inizializzo variabili di wrk
047000020204     C                   Z-ADD     *zeros        Num5_0            5 0
047100101018     C                   MOVEL     *blanks       wFlgCAS
047200060331     C                   MOVEL     *blanks       wVATNOT_A
047300060331     C                   MOVEL     *blanks       wVATNOT_B
047400060331     C                   MOVEL     *blanks       wVATNOT_E
047500110927     C                   MOVEL     *blanks       wVATNOT_I
047600120328     C                   MOVEL     *blanks       wNOT             70
047700030822     C*
047800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047900030822     C                   CLEAR                   SkSplitCSV
048000030822     C                   EVAL      i = 1
048100030822     C                   EVAL      posDa = *zeros
048200030822     C                   EVAL      posA  = *zeros
048300080527     C*
048400080527     C                   CLEAR                   FIVABDS
048500080527     C                   CLEAR                   FIVAT000
048600030822     C*
048700030822     C* Reimposto i valori di default
048800030822     C                   EXSR      DEFCAM
048900030822     C*
049000010330     C                   ENDSR
049100010330     C*----------------------------------------------------*
049200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049300010330     C*----------------------------------------------------*
049400010330     C     DEFCAM        BEGSR
049500030822     C*
049600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049700030715     C* e delimitatore testo.
049800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050100100318     C*
050200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050300100318     C                   EVAL      CharSOS = CharNUM
050400030822     C*
050500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050600100720     C                   SETOFF                                       505152
050700100720     C                   SETOFF                                       53
050800100413     C                   SELECT
050900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051000100720     C                   SETON                                        50
051100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051200100413     C                   SETON                                        51
051300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051400100720     C                   SETON                                        52
051500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051600100720     C                   SETON                                        53
051700100413     C                   ENDSL
051800030822     C*
051900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052000030822     C                   EVAL      posDaDft = 1
052100030822     C                   EVAL      posADft  = 0
052200030822     C                   EVAL      wGiroDft = 0
052300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052400030822     C                             posDaDft > 0
052500030822     C*
052600030822     C* Gestisco il 1° giro
052700030822     C                   IF        wGiroDft = 0
052800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053000030822     C* Incremento il contatore dei "giri"
053100030822     C                   EVAL      wGiroDft = 1
053200030822     C                   ELSE
053300030822     C                   EVAL      posDaDft = posADft
053400030822     C                   ENDIF
053500030822     C* Eseguo lo scan x trovare la fine del campo corrente
053600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053700030822     C*
053800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054000030822     C                   IF        posDaDft > 0 AND
054100030822     C                             posADft  > 0
054200030822     C* NCL
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'NCL'
054700030822     C                   EVAL      PiStr=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   EXSR      CHKNUM
055100030822     C                   IF        PiInt=*on
055200030822     C                   Z-ADD     PiVal         VABNCL
055300030822     C                   ENDIF
055400030822     C                   ENDIF
055500030822     C* CCM
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'CCM'
056000030822     C                   EVAL      PiStr=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   EXSR      CHKNUM
056400030822     C                   IF        PiInt=*on
056500030822     C                   Z-ADD     PiVal         VABCCM
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* LNP
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'LNP'
057300030822     C                   EVAL      PiStr=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   EXSR      CHKNUM
057700030822     C                   IF        PiInt=*on
057800030822     C                   Z-ADD     PiVal         VABLNP
057900030822     C                   ENDIF
058000030822     C                   ENDIF
058100030822     C* NRS
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'NRS'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiInt=*on
059100030822     C                   Z-ADD     PiVal         VABNRS
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* CTR
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'CTR'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiInt=*on
060400030822     C                   Z-ADD     PiVal         VABCTR
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* PKB
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'PKB'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiNum=*on
061700030822     C                   Z-ADD     PiVal         VABPKB
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* VLB
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'VLB'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiNum=*on
063000030822     C                   Z-ADD     PiVal         VABVLB
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* QFT
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'QFT'
063800030822     C                   EVAL      PiStr=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   EXSR      CHKNUM
064200030822     C                   IF        PiNum=*on
064300030822     C                   Z-ADD     PiVal         VABQFT
064400030822     C                   ENDIF
064500030822     C                   ENDIF
064600030822     C* CBO
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'CBO'
065100030822     C                   EVAL      VABCBO=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* TSP
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'TSP'
066000030822     C                   EVAL      VABTSP=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* VAS
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'VAS'
066900030822     C                   EVAL      VABVAS=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* VCA
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'VCA'
067800030822     C                   EVAL      VABVCA=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200070925     C* TIC
068300070925     C                   IF        %subst(
068400070925     C                             %subst(vlrppt:posDaDft+1:
068500070925     C                             posADft-posDaDft-1):1:3)
068600070925     C                             = 'TIC'
068700070925     C                   EVAL      VABTIC=%trim(%subst(
068800070925     C                             %subst(vlrppt:posDaDft+1:
068900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000070925     C                   ENDIF
069100030822     C* GCA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'GCA'
069600030822     C                   EVAL      VABGCA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* CTM
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'CTM'
070500030822     C                   EVAL      VABCTM=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* FFD
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'FFD'
071400030822     C                   EVAL      VABFFD=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* VAD
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VAD'
072300030822     C                   EVAL      VABVAD=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* GMA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'GMA'
073200030822     C                   EVAL      VABGMA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* GGA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GGA'
074100030822     C                   EVAL      VABGGA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* GVA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GVA'
075000030822     C                   EVAL      VABGVA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* TC1
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'TC1'
075900030822     C                   EVAL      VABTC1=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* TC2
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'TC2'
076800030822     C                   EVAL      VABTC2=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200040714     C* VATTRC
077300040714     C                   IF        %subst(
077400040714     C                             %subst(vlrppt:posDaDft+1:
077500040714     C                             posADft-posDaDft-1):1:3)
077600040714     C                             = 'TRC'
077700040714     C                   EVAL      VATTRC=%trim(%subst(
077800040714     C                             %subst(vlrppt:posDaDft+1:
077900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000040714     C                   ENDIF
078100030822     C* ...
078200030822     C                   ENDIF
078300030822     C                   ENDDO
078400020204     C*
078500010330     C                   ENDSR
078600010607     C*----------------------------------------------------*
078700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078800010607     C*----------------------------------------------------*
078900010607     C     IMPVAB        BEGSR
079000010607     C*
079100010607     C                   SETOFF                                       3132
079200110315 xxx C***                SETON                                        35
079300030515     C*
079400010607     C                   MOVEL     datcor        VABAAS
079500010607     C                   MOVE      datcor        VABMGS
079600010607     C*
079700030715     C**********
079800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079900030715     C**********
080000030715     C                   IF        wGiro = *zeros
080100040113     C*
080200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080500080429     C                             %trim(intcol(5))<>*blanks
080600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
080800080429     C                                      %trim(intcol(5))
080900070823     C     ';':CharCSV   XLATE     vindta        vindta
081000040510     C                   ENDIF
081100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081200031201     C     minu:maiu     XLATE     vindta        vindta
081300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081400030715     C                   DOW       posDa <= %len(%trim(vindta))
081500030715     C*
081600030715     C* Gestisco il 1° campo
081700030715     C                   IF        i = 1
081800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081900030715     C                   EVAL      posDa = 1
082000030715     C* Eseguo lo scan x trovare la fine del primo campo
082100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082200030715     C* A questo punto "estraggo" il campo corrente
082300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082400030715     C                                                    (posA-posDa))
082500030715     C* X i campi successivi al 1°
082600030715     C                   ELSE
082700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082800030715     C                   EVAL      posDa = posA + 1
082900030715     C* Eseguo lo scan x trovare la fine del campo corrente
083000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083100030715     C* Gestisco l'ultimo campo
083200030715     C                   IF        posA = *zeros
083300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083400030715     C* A questo punto "estraggo" il campo corrente
083500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083600030715     C                                                    (posA-posDa))
083700030715     C                   LEAVE
083800030715     C                   ELSE
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C                   ENDIF
084300030715     C                   ENDIF
084400030715     C* Incremento il contatore d campo
084500030715     C                   EVAL      i = i +1
084600030715     C                   ENDDO
084700030715     C*
084800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084900030715     C* delimitazione testo
085000030715     C                   EVAL      i = 1
085100030715     C                   DOW       i < %elem(SkSplitFLD)
085200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085400030715     C                   EVAL      i = i + 1
085500030715     C                   ENDDO
085600030715     C                   EVAL      wGiro = 1
085700030715     C                   ELSE
085800030715     C                   EVAL      wGiro = 2
085900031204     C**********
086000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086100031204     C* d separatore campo
086200031204     C**********
086300031204     C                   EVAL      vindta = %trim(vindta)
086400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086500031204     C                   EVAL      vindta = %subst(vindta:2)
086600031204     C                   ENDDO
086700031204     C*
086800031204     C                   Z-ADD     *zeros        lunghInput        4 0
086900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087300110912     C                   ENDDO
087400030715     C**********
087500030715     C* Effettuo lo split del campo dati d input
087600030715     C**********
087700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087800030715     C                   DOW       posDa <= %len(%trim(vindta))
087900030715     C*
088000030715     C* Gestisco il 1° campo
088100030715     C                   IF        i = 1
088200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088300030715     C                   EVAL      posDa = 1
088400030715     C* Eseguo lo scan x trovare la fine del primo campo
088500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088600030715     C* A questo punto "estraggo" il campo corrente
088700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088800030715     C                                                    (posA-posDa))
088900030715     C* X i campi successivi al 1°
089000030715     C                   ELSE
089100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089200030715     C                   EVAL      posDa = posA + 1
089300031204     C* Verifico che nn vi sia il campo nn valorizzato
089400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089500031204     C* Se campo nn valorizzato skippo al prossimo
089600031204     C                   EVAL      SkSplitCSV(i) = *blanks
089700031204     C                   EVAL      posA  = posA + 1
089800031204     C                   ELSE
089900030715     C* Eseguo lo scan x trovare la fine del campo corrente
090000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090100030715     C* Gestisco l'ultimo campo
090200030715     C                   IF        posA = *zeros
090300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090400030715     C* A questo punto "estraggo" il campo corrente
090500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090600030715     C                                                    (posA-posDa))
090700030715     C                   LEAVE
090800030715     C                   ELSE
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   ENDIF
091300031204     C                   ENDIF
091400030715     C                   ENDIF
091500030715     C* Incremento il contatore d campo
091600030715     C                   EVAL      i = i +1
091700030715     C                   ENDDO
091800030715     C*
091900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092000030715     C* delimitazione testo
092100030715     C                   EVAL      i = 1
092200030715     C                   DOW       i < %elem(SkSplitCSV)
092300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092500030715     C                   EVAL      i = i + 1
092600030715     C                   ENDDO
092700100720     C*
092800100720     C* Stacco un progressivo x numero spedizione
092900120301 xxx C                   EXSR      repNSP
093000100326     C*
093100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093200030715     C                   EVAL      i = 1
093300030715     C                   DOW       i < %elem(SkSplitFLD)
093400030715     C*
093500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093700030715     C                             SkSplitCSV(i) <> *blanks
093800030721     C*** ATB
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094000030721     C                   EVAL      VABATB = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030715     C*** RSD
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094700030715     C* ==
094800030715     C                   ENDIF
094900030721     C*** RD2
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030715     C*** IND
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095500030715     C                   EVAL      VABIND = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030715     C*** LOD
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030715     C*** PRD
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030721     C*** NZD
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030715     C*** RMA
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** NOT
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030715     C*** NT2
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098000030715     C                   ENDIF
098100030715     C*** VCA
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030715     C*** VAS
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** FFD
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030721     C*** GC1
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** GC2
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** TSP
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100400030721     C                   ENDIF
100500040510     C*** CBO
100600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100800040510     C                   ENDIF
100900030721     C*** NAS
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** TIC
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** GCA
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** XCO
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** CTM
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TCR
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** CTS
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** FTM
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** VAD
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** GMA
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** GGA
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** GVA
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** TC1
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** TC2
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** SCL
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** RMO
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** NMO
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*
107800030721     C* Reperisco quindi i campi numerici...
107900040510     C*** NSP
108000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108100040510     C                   EVAL      PiStr=SkSplitCSV(i)
108200040510     C                   EXSR      CHKNUM
108300040510     C                   IF        PiInt=*on
108400040510     C                   Z-ADD     PiVal         VABNSP
108500040510     C                   ELSE
108600040510     C                   SETON                                        31
108700040510     C                   EVAL      VABNSP = *zeros
108800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108900040510     C                             + ' ' + 'VABNSP'
109000040510     C                   ENDIF
109100040510     C                   ENDIF
109200071106     C*** RMN
109300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109400071106     C                   EVAL      PiStr=SkSplitCSV(i)
109500071106     C                   EXSR      CHKNUM
109600071106     C                   IF        PiInt=*on
109700071106     C                   Z-ADD     PiVal         VABRMN
109800071106     C                   ELSE
109900071106     C                   SETON                                        32
110000071106     C                   EVAL      VABRMN = *zeros
110100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110200071106     C                             + ' ' + 'VABRMN'
110300071106     C                   ENDIF
110400071106     C                   ENDIF
110500030721     C*** CAD
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiInt=*on
111000030721     C                   Z-ADD     PiVal         Num5_0
111100030721     C                   MOVEL(p)  Num5_0        VABCAD
111200030721     C                   ELSE
111300030721     C                   SETON                                        32
111400030721     C                   EVAL      VABCAD = *zeros
111500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111600030721     C                             + ' ' + 'VABCAD'
111700030721     C                   ENDIF
111800030721     C                   ENDIF
111900030721     C*** NCL
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiInt=*on
112400030721     C                   Z-ADD     PiVal         VABNCL
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700030721     C                   EVAL      VABNCL = *zeros
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900040122     C                             + ' ' + 'VABNCL'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** PKB
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiNum=*on
113700030721     C                   Z-ADD     PiVal         VABPKB
113800030721     C                   ELSE
113900030721     C                   SETON                                        32
114000030721     C                   EVAL      VABPKB = *zeros
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200030721     C                             + ' ' + 'VABPKB'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** CAS
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114700110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
114800111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
114900111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
115000120227 xxx C                   EVAL      VABvca = 'EUR'
115100101018     C                   EVAL      wFlgCAS = '1'
115200101018     C                   ENDIF
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiNum=*on
115600030721     C                   Z-ADD     PiVal         VABCAS
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABCAS = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABCAS'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** CCM
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on      AND
116900030721     C                             PiVal<=9999999 AND
117000030721     C                             PiVal>*zeros
117100030721     C                   Z-ADD     PiVal         VABCCM
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABCCM = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600040510     C                             + ' ' + 'VABCCM'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** LNP
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on    AND
118400030721     C                             PiVal<=999   AND
118500030721     C                             PiVal>*zeros
118600030721     C                   Z-ADD     PiVal         VABLNP
118700030721     C                   ELSE
118800040715     C                   SETON                                        32
118900030721     C                   EVAL      VABLNP = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100040510     C                             + ' ' + 'VABLNP'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** NRS
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on AND
119900030721     C                             PiVal<=99
120000030721     C                   Z-ADD     PiVal         VABNRS
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABNRS'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** LNA
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on    AND
121200030721     C                             PiVal<=999   AND
121300030721     C                             PiVal>*zeros
121400030721     C                   Z-ADD     PiVal         VABLNA
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      VABLNA = *zeros
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABLNA'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** CTR
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on  AND
122700030721     C                             PiVal<=999
122800030721     C                   Z-ADD     PiVal         VABCTR
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABCTR'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** IAS
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiNum=*on
124000030721     C                   Z-ADD     PiVal         VABIAS
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABIAS'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** VLB
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiNum=*on
125200030721     C                   Z-ADD     PiVal         VABVLB
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABVLB'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** QFT
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiNum=*on
126400030721     C                   Z-ADD     PiVal         VABQFT
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABQFT'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** NCD
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on      AND
127600030721     C                             PiVal<=9999999
127700030721     C                   Z-ADD     PiVal         VABNCD
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABNCD'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** NCA
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on      AND
128900030721     C                             PiVal<=9999999
129000030721     C                   Z-ADD     PiVal         VABNCA
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABNCA'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** ZNC
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on AND
130200030721     C                             PiVal<=99
130300030721     C                   Z-ADD     PiVal         VABZNC
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABZNC'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** DCR
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on       AND
131500030721     C                             PiVal<=99999999
131600030721     C                   Z-ADD     PiVal         VABDCR
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABDCR'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** HCR
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on   AND
132800030721     C                             PiVal<=9999
132900030721     C                   Z-ADD     PiVal         VABHCR
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABHCR'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** VMD
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiNum=*on
134100030721     C                   Z-ADD     PiVal         VABVMD
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABVMD'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** ANT
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on        AND
135300030721     C                             PiVal<=999999999
135400030721     C                   Z-ADD     PiVal         VABANT
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABANT'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** CMO
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on
136600030721     C                   Z-ADD     PiVal         Num5_0
136700030721     C                   MOVEL(p)  Num5_0        VABCMO
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      VABCMO = *zeros
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABCMO'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500040510     C*
137600040510     C* Reperisco i campi del FIVAT
137700040510     C*** VATNOT
137800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138000040510     C                   ENDIF
138100040510     C*** VATTRC
138200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138400040510     C                   ENDIF
138500060331     C*** VATNOT_A
138600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138800060331     C                   ENDIF
138900060331     C*** VATNOT_B
139000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139200060331     C                   ENDIF
139300060331     C*** VATNOT_E
139400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139600060331     C                   ENDIF
139700110927     C*** VATNOT_I
139800110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
139900110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140000110927     C                   ENDIF
140100070823     C*
140200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140300120301
140400120320      *** W_VABRSD = VABRSD + VABRD2
140500120320     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
140600120320     C                   MOVEL     *blanks       wRSD             70
140700120320     C                   EVAL      wRSD=%trim(SkSplitCSV(i))
140800120320     C                   EVAL      VABRSD=%subst(wRSD:1:35)
140900120320     C                   EVAL      VABRD2=%subst(wRSD:36:35)
141000120301     C                   ENDIF
141100070925     C*
141200071029     C***  <===  -----------------------------------------
141300040510     C*
141400030715     C                   ENDIF
141500030715     C                   EVAL      i = i + 1
141600030715     C                   ENDDO
141700070925     C*
141800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
141900100831     C*
142000070925     C*
142100071029     C***  <===  -----------------------------------------
142200070925     C*
142300070925     C* Considerazioni finali su CBO/CAS
142400100318     C                   IF        wFlgCAS = '1'    OR
142500100318     C                             VABCAS  > *zeros
142600070925     C                   IF        VABCBO = '1'
142700070925     C                   EVAL      VABCBO = '4'
142800070925     C                   ENDIF
142900070925     C                   IF        VABCBO = '2'
143000070925     C                   EVAL      VABCBO = '6'
143100070925     C                   ENDIF
143200070925     C                   ENDIF
143300070925     C***  <===  ----------------------------
143400040510     C*
143500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
143600040510     C                   IF        VABNSP = *zeros
143700040510     C                   SETON                                        31
143800040510     C                   ENDIF
143900040715     C*
144000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
144100040715     C                   IF        VABLNP = *zeros
144200040715     C                   SETON                                        31
144300040715     C                   ENDIF
144400040510     C*
144500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
144600120410     C***                IF        VABRMN = *zeros
144700120410     C***                Z-ADD     VABNSP        VABRMN
144800120410     C***                ENDIF
144900120328     C* IN QUESTO CASO LO FORZO = 1
145000120328     C                   IF        VABRMN = *zeros
145100120328     C                   Z-ADD     1             VABRMN
145200120328     C                   ENDIF
145300040510     C*
145400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
145500040510     C                   IF        VABRMA = *blanks
145600040714     C                   MOVEL     VABRMN        VABRMA
145700040510     C                   ENDIF
145800040128     C*
145900040510     C* Se provincia nn valorizzata la reperisco
146000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146100110912     C                   IF        VABPRD =  *blanks AND
146200110912     C                             VABNZD =  *blanks AND
146300110912     C                             VABCAD <> *blanks AND
146400110912     C                             VABLOD <> *blanks
146500040128     C                   CLEAR                   TISI95DS
146600040128     C                   EVAL      I95TCN = '3'
146700040128     C                   Z-ADD     datcor        I95DAT
146800040128     C                   EVAL      I95NAR = VABNZD
146900040128     C                   EVAL      I95CAP = VABCAD
147000040128     C                   EVAL      I95LOC = VABLOD
147100040128     C                   CALL      'TISI95R'
147200040128     C                   PARM                    TISI95DS
147300040128     C                   EVAL      VABPRD = O95PRV
147400040510     C                   ENDIF
147500020204     C*
147600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
147700020204     C                   EXSR      CHKIMPDIV
147800030715     C                   ENDIF
147900020204     C*
148000010607     C                   ENDSR
148100010607     C*----------------------------------------------------*
148200010601
148300020204
148400020204     C*----------------------------------------------------*
148500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
148600020204     C*----------------------------------------------------*
148700020204     C     CHKIMPDIV     BEGSR
148800020204     C*
148900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149000020204     C                   Z-ADD     *zeros        wrkDec            9 9
149100020204     C*
149200020204     C* Come prima cosa effettuo considerazioni sulla divisa
149300020204     C                   IF        vabIAS > *zeros
149400020204     C                   IF        vabVAS <> 'EUR'
149500020204     C                   EVAL      vabVAS =  'ITL'
149600020204     C                   ENDIF
149700020204     C                   ENDIF
149800020204     C*
149900020204     C                   IF        vabCAS > *zeros
150000020204     C                   IF        vabVCA <> 'EUR'
150100020204     C                   EVAL      vabVCA =  'ITL'
150200020204     C                   ENDIF
150300020204     C                   ENDIF
150400020204     C*
150500020204     C                   IF        vabVMD > *zeros
150600020204     C                   IF        vabVAD <> 'EUR'
150700020204     C                   EVAL      vabVAD =  'ITL'
150800020204     C                   ENDIF
150900020204     C                   ENDIF
151000020204     C*
151100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
151200020204     C                   Z-ADD     vabIAS        wrkDec
151300020204     C                   IF        wrkDec > *zeros
151400020204     C                   IF        vabVAS = 'ITL'
151500020204     C                   EVAL      vabIAS = *zeros
151600020204     C                   ENDIF
151700020204     C                   ENDIF
151800020204     C*
151900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152000020204     C                   Z-ADD     vabCAS        wrkDec
152100020204     C                   IF        wrkDec > *zeros
152200020204     C                   IF        vabVCA = 'ITL'
152300020204     C                   EVAL      vabCAS = *zeros
152400020204     C                   ENDIF
152500020204     C                   ENDIF
152600020204     C*
152700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
152800020204     C                   Z-ADD     vabVMD        wrkDec
152900020204     C                   IF        wrkDec > *zeros
153000020204     C                   IF        vabVAD = 'ITL'
153100020204     C                   EVAL      vabVMD = *zeros
153200020204     C                   ENDIF
153300020204     C                   ENDIF
153400020204     C*
153500020204     C                   ENDSR
153600020204     C***
153700010330
153800100318
153900100318
154000100318     C*----------------------------------------------------*
154100100318     C*  CONTROLLO NUMERICITA' CAMPI
154200100318     C*----------------------------------------------------*
154300100318     C     CHKNUM        BEGSR
154400100318     C*
154500100318     C                   IF        PiDecChr = *blanks
154600100318     C                   EVAL      PiDecChr = CharNUM
154700100318     C                   ENDIF
154800100318     C*
154900100318     C                   callp     UBISNUM_Check(PiStr
155000100318     C                                          :PiDecChr
155100100318     C                                          :PiVal
155200100318     C                                          :PiNum
155300100318     C                                          :PiInt)
155400100318     C*
155500100318     C                   ENDSR
155600100318     C***
155700010601
155800010601
155900010601
156000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156100010601     C     repfil        BEGSR
156200010601     C*
156300010601     C                   if        invfil = *zeros and
156400010601     C                             depfil > *zeros and
156500010629     C                             (vinflg = *blanks or
156600010629     C                              vinflg = *zeros)
156700010601     C
156800010601     C                   eval      invfil = depfil
156900010601     C                   endif
157000010601     C*
157100010601     C                   if        depfil <> invfil and
157200010601     C                             invfil > *zeros
157300010601     C                   eval      flgMulti = '1'
157400010601     C                   if        vinflg = *blanks
157500010601     C                   add       1             cntNonEl
157600010601     C                   endif
157700010601     C                   endif
157800010601     C*
157900010601     C                   if        vinflg = '2'
158000010601     C                   eval      flgStato = '2'
158100010601     C                   endif
158200010601     C*
158300010601     C                   ENDSR
158400010601     C***
158500010601
158600010601
158700010601
158800010330
158900040119      /TITLE Invio dei dati al punto operativo.
159000040119     C     invio         BEGSR
159100040119     C*
159200040119     C* 1° invio FIVAT
159300040119     C                   reset                   dscmz
159400040119     C                   move      vlrpoi        cmzdst
159500040119     C                   eval      cmzfld = 'FIVATWWR'
159600040119     C                   eval      cmzmbd = vlrhdl
159700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159800040119     C***                if        prmfir = *blanks
159900040119     C                   eval      cmzfla = 'FIVAT00F'
160000040119     C                   eval      cmzmba = 'FIVAT00F'
160100040119     C***                else
160200040119     C***                eval      cmzfla = prmfir
160300040119     C***                eval      cmzmba = prmfir
160400040119     C***                endif
160500040119     C                   eval      cmznrr = *zeros
160600040119     C                   move      §ctrokvt      cmznrr
160700040119     C                   eval      cmzlba = vlrfl1
160800040119     C                   call(e)   'TIS711C'
160900040119     C                   parm                    dscmz
161000040119     C                   parm      *blanks       esito
161100040119     C                   if        %error
161200040119     C                             or cmzerr = '1'
161300040119     C                             or esito  = '1'
161400040119     C                   eval      wrkesito = '3'
161500040119     C                   else
161600040119     C*
161700040119     C* 2° invio FIVAB
161800040119     C                   reset                   dscmz
161900040119     C                   move      vlrpoi        cmzdst
162000040119     C                   eval      cmzfld = vlrfou
162100040119     C                   eval      cmzmbd = vlrhdl
162200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162300040119     C***                if        prmfir = *blanks
162400040119     C                   eval      cmzfla = 'FIVAB00F'
162500040119     C                   eval      cmzmba = 'FIVAB00F'
162600040119     C***                else
162700040119     C***                eval      cmzfla = prmfir
162800040119     C***                eval      cmzmba = prmfir
162900040119     C***                endif
163000040119     C                   eval      cmznrr = *zeros
163100040119     C                   move      §ctrokvb      cmznrr
163200040119     C                   eval      cmzlba = vlrfl1
163300040119     C                   call(e)   'TIS711C'
163400040119     C                   parm                    dscmz
163500040119     C                   parm      *blanks       esito
163600040119     C                   if        %error
163700040119     C                             or cmzerr = '1'
163800040119     C                             or esito  = '1'
163900040119     C                   eval      wrkesito = '3'
164000040119     C                   endif
164100040119     C                   endif
164200040119     C*
164300040119     C                   ENDSR
164400040119     C***
164500010601
164600010601
164700010601
164800010601
164900010601      /TITLE Invio dei dati al punto operativo.
165000010601     C     opeini        BEGSR
165100010601     C*
165200010601     C* Inizializzo flag e contatori operativi
165300010601     C                   movel     '0'           flgGiro           1
165400010601     C                   movel     '0'           flgMulti          1
165500010601     C                   movel     '1'           flgStato          1
165600010615     C                   movel     '0'           flgOk             1
165700010601     C                   z-add     *zeros        cntNonEl         10 0
165800010601     C                   z-add     *zeros        depfil            3 0
165900010601     C                   z-add     *zeros        invfil            3 0
166000010601     C*
166100010601     C                   ENDSR
166200010601     C***
166300070326
166400070326
166500070326
166600070326
166700070326     C     *pssr         BEGSR
166800070329     C*
166900070329     C                   if        %open(tivin00r)
167000070329     C                   close     tivin00r
167100070329     C                   endif
167200070329     C                   if        %open(fivabwwr)
167300070329     C                   close     fivabwwr
167400070329     C                   endif
167500070329     C                   if        %open(fivatwwr)
167600070329     C                   close     fivatwwr
167700070329     C                   endif
167800070326     C*
167900070326     C* Effettuo la chiamata al CLLE preposto
168000070326     C                   call(e)   'TITVVTC'
168100070326     C                   parm                    parccm
168200070326     C                   parm                    parmbr
168300070326     C                   parm      '2'           paropz
168400070326     C*
168500070326     C                   eval      wrkesito = '2'
168600070404     C*
168700070404     C                   seton                                        LR
168800070326     C*
168900070326     C                   ENDSR     '*CANCL'
169000070326     C***
169100100318
169200100318
169300100318
169400100318     C     normCSV       BEGSR
169500100318     C*
169600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
169700100318     C* separatori campi
169800100318     C                   if        CharTXT <> *blanks
169900100318     C                   z-add     *zeros        wLenVINDTA        4 0
170000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
170100100318     C                   setoff                                       80
170200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
170300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
170400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
170500100318     C                   if        not *in80
170600100318     C                   seton                                        80
170700100318     C                   else
170800100318     C                   setoff                                       80
170900100318     C                   endif
171000100318     C                   endif
171100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
171200100318     C                             and *in80 = *on
171300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
171400100318     C                   endif
171500100318     C                   enddo
171600100318     C                   endif
171700100318     C*
171800100318     C                   ENDSR
171900100318     C***
172000070326
172100100326
172200100326
172300100326     C*----------------------------------------------------*
172400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
172500100326     C*----------------------------------------------------*
172600100326     C     REPNSP        BEGSR
172700100326     C*
172800100326     C* NSP => Stacco un numeratore da AZNUM
172900100326     C                   clear                   TRUL33DS
173000100326     C                   eval      I33OPE = *zeros
173100100326     C                   eval      I33CNU = 302
173200100326     C                   eval      I33NUM = 1
173300100326     C                   movel     TRUL33DS      KPJBU
173400100326     C                   call      'TRUL33R'
173500100326     C                   parm                    KPJBA
173600100326     C                   movel     KPJBU         TRUL33DS
173700100326     C                   if        O33ERR = *zeros
173800100326     C                   z-add     O33NRF        VABNSP
173900100326     C                   z-add     O33NRF        VATNSP
174000100326     C                   else
174100100326     C                   SETON                                        31
174200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
174300100326     C                             + ' ' + 'VABNSP VATNSP'
174400100326     C                   endif
174500100326     C*
174600100326     C                   ENDSR
174700010330
174800100326
174900010330
175000000613     C     *inzsr        BEGSR
175100990910     C*
175200990910     C     *entry        plist
175300990920     C                   parm                    tivlrds
175400990921     C                   parm      wrkesito      esito
175500000724     C                   parm                    prmlit
175600000710     C                   parm                    prmfir
175700010330     C*
175800010330     C* CALCOLA LA DATA CORRENTE
175900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
176000100318     C                   eval      datcor = %dec(%date() : *ISO)
176100000613     C*
176200000613     C                   ENDSR
176300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
176400120417VABRMN;X;W_VABRSD;VABIND;VABCAD;VABLOD;VATNOT_B;VABNCL;VABPKB;VATNOT_E
176500120227
176600120410
176700111116
176800100318
