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
003600120418     D Intestazioni    s               n   inz(*off)
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
006300120411     D wNOT            s             70
006400120411     D Data8A          s              8
006500030822     D*------------------
006600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006700030822     D*------------------
006800030822     D posDaDft        S              3  0 INZ(*zeros)
006900030822     D posADft         S              3  0 INZ(*zeros)
007000030822     D j               s              3  0 INZ(1)
007100030822     D wGiroDft        s              1  0 INZ(*zeros)
007200080418     D*------------------
007300080418     D* Costanti
007400080418     D*------------------
007500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007700100318
007800100318     D*------------------
007900100318     D* LINKING A DEFINIZIONI ESTERNE
008000100318     D*------------------
008100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008300100318
008400010330
008500010330
008600010330
008700000913     C                   reset                   rrnum
008800990921     C                   reset                   esito
008900990921     C                   reset                   wrkesito
009000010601     C*
009100010601     C                   exsr      opeini
009200010605     C                   exsr      rwvab
009300010601     C*
009400010601     C                   seton                                        lr
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010000010601     C*--------------------------------------------------------
010100010601     C     PREELA        BEGSR
010200010601     C*
010300010601     C* SE OCCORRE SPEDIRE IN FILIALE
010400010601     C                   if        invfil <> *zeros and
010500010601     C                             flgGiro = '0'
010600010601     C*
010700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010800010601     C                   eval      flgGiro = '1'
010900010601     C*
011000010601     C                   endif
011100010601     C*
011200010601     C                   ENDSR
011300010601     C***
011400010601
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010601     C*--------------------------------------------------------
012100010601     C     ENDELA        BEGSR
012200040119     C*
012300040119     C* Effettuo la chiamata al CLLE preposto
012400040119     C                   call(e)   'TITVVTC'
012500040510     C                   parm                    parccm
012600040119     C                   parm                    parmbr
012700040119     C                   parm      '2'           paropz
012800050201     C*
012900050201     C* Effettuo lancio TISI95 solo x chiusura
013000050201     C                   CLEAR                   TISI95DS
013100050201     C                   EVAL      I95TLA = 'C'
013200050201     C                   CALL      'TISI95R'
013300050201     C                   PARM                    TISI95DS
013400000616     C*
013500010601     C                   ENDSR
013600010601
013700010601
013800010330     C*--------------------------------------------------------
013900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014000010330     C*--------------------------------------------------------
014100010605     C     RWVAB         BEGSR
014200010330     C*
014300010330     C                   if        not %open(tivin00r)
014400010330     C                   open      tivin00r
014500010330     C                   endif
014600021025     C                   if        not %open(fivabwwr)
014700021025     C                   open      fivabwwr
014800010330     C                   endif
014900040119     C*
015000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015100040119     C                   exsr      prevat
015200040119     C*
015300040119     C                   if        chkcall = '0'
015400040119     C*
015500040119     C                   if        not %open(fivatwwr)
015600040119     C                   open      fivatwwr
015700040119     C                   endif
015800010330     C*
015900040119     C                   clear                   §CTROKVB          7 0
016000040119     C                   clear                   §CTROKVT          7 0
016100040119     C                   clear                   §CTRMO            7 0
016200040119     C                   clear                   §CTRNO            7 0
016300120410     C*
016400120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
016500120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
016600120410     C                   IF        Intestazioni = *off
016700120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016800120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016900120410     C                   exsr      inzvar
017000120410     C                   exsr      impvab
017100120410     C                   ENDIF
017200010330     C*
017300030822     C                   do        *HIVAL
017400010330     C*
017500030822     C                   read      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100040510     C*
018200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018300040510     C                   z-add     *zeros        wLenVINDTA        4 0
018400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018800040510     C                   z-add     1             wFlgVINDTA
018900040510     C                   leave
019000040510     C                   endif
019100040510     C                   enddo
019200010618     C*
019300040510     C                   if        vindta > *blanks AND
019400040510     C                             wFlgVINDTA = 1
019500010330     C                   add       1             rrnum
019600010330     C*
019700010601     C                   if        *in70 = *off and
019800010330     C                             (vinflg = *blanks
019900010330     C                              or vinflg = '0'
020000010330     C                              or vinflg = '2')
020100010330     C*
020200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020300010711     C                   if        vinflg = *blanks or vinflg = '0'
020400010711     C                   clear                   vinmsg
020500010711     C                   endif
020600010601     C*
020700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020800070925     C                   eval      vindta  = ' ' + %trim(vindta)
020900070925     C*
021000080527     C                   exsr      inzvar
021100100318     C                   exsr      normCSV
021200010605     C                   exsr      impvab
021300010601     C*
021400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021500010605     C                   eval      depfil = VABLNP
021600010601     C                   exsr      repfil
021700010601     C                   if        depfil = invfil
021800021025     C                   if        vlrpoi = 999
021900030822     C                   move(P)   invfil        VABFGS
022000021025     C                   else
022100030822     C                   move(P)   vlrpoi        VABFGS
022200021025     C                   endif
022300010601     C*
022400010601     C                   exsr      PREELA
022500010601     C*
022600010604     C* Ebbene...
022700010604     C*
022800040119     C  N31              add       1             §CTROKVB
022900040119     C   32              add       1             §CTRMO
023000040119     C   31              add       1             §CTRNO
023100080418     C*
023200030822     C                   if        wGiro = 2
023300080513     C* Scrivo sempre il VAT
023400080513     C  N31              exsr      wrivat
023500110201     C                   exsr      chkvab
023600080513     C                   endif
023700010604     C*
023800010604     C                   if        *in31 = *off and
023900010604     C                             *in32 = *off
024000010604     C                   eval      vinflg = '1'
024100010604     C                   else
024200010604     C                   eval      vinflg = '2'
024300010604     C                   endif
024400010604     C                   endif
024500010601     C*
024600010604     C                   endif
024700010604     C*
024800010330     C                   else
024900010330     C                   eval      vinflg = '1'
025000010330     C                   endif
025100010601     C*
025200010601     C  N70              update    tivin000
025300010330     C*
025400030822     C  N70              enddo
025500040119     C*
025600080527     C* Verifico dati rimasti "in cannna"...
025700080527     C                   seton                                        65
025800110201     C                   exsr      chkvab
025900080527     C*
026000040119     C                   endif                                                  (endif - chkcall)
026100010601     C*
026200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026300010601     C                   if        cntNonEl = *zeros or
026400010601     C                             flgMulti = '0'
026500010330     C* Se non ci sono record con errori ...
026600010601     C                   if        §ctrno = 0 and
026700010604     C                             §ctrmo = 0 and
026800010601     C                             flgStato <> '2'
026900010330     C* ... restituisco esito OK.
027000010330     C                   eval      wrkesito = '0'
027100010330     C                   else
027200040119     C                   if        §ctrokvb > 0
027300010330     C                   eval      wrkesito = '1'
027400010330     C                   else
027500010615     C                   if        flgOk = '0'
027600010615     C                   eval      wrkesito = '2'
027700010615     C                   else
027800010615     C                   eval      wrkesito = '6'
027900010615     C                   endif
028000010330     C                   endif
028100010330     C                   endif
028200010601     C                   else
028300010601     C                   eval      wrkesito = '9'
028400010601     C                   endif
028500010330     C*
028600010330     C                   if        %open(tivin00r)
028700010330     C                   close     tivin00r
028800010330     C                   endif
028900021025     C                   if        %open(fivabwwr)
029000021025     C                   close     fivabwwr
029100010330     C                   endif
029200040512     C                   if        %open(fivatwwr)
029300040512     C                   close     fivatwwr
029400040512     C                   endif
029500010601     C*
029600010601     C                   if        vlrpoi <> 999
029700010601     C                   eval      invfil = vlrpoi
029800010601     C                   endif
029900010330     C*
030000040119     C                   if        §ctrokvb > 0
030100010601     C                             and invfil > *zeros
030200010330     C                   exsr      invio
030300010330     C                   endif
030400010601     C*
030500010618     C                   if        flgGiro = '1'
030600010601     C                   exsr      endela
030700010618     C                   endif
030800010330     C*
030900010330     C                   ENDSR
031000010330     C***
031100080527
031200080527
031300080527
031400080527     C*----------------------------------------------------*
031500080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031600080527     C*----------------------------------------------------*
031700100413     C     CHKVAB        BEGSR
031800110201     C*
031900110201     C* Se richiesti ignorare colli / peso / volume
032000110315 xxx C***                if        *in35
032100110315 xxx C***                z-add     *zeros        VABNCL
032200110315 xxx C***                z-add     *zeros        VABPKB
032300110315 xxx C***                z-add     *zeros        VABVLB
032400110315 xxx C***                endif
032500080527     C*
032600080527     C* Scrivo il VAB solo a rottura d spedizione
032700080527     C                   if        depspe = *zeros
032800080527     C  N31              eval      fivabds_sav = fivabds
032900080527     C                   eval      depspe = vabNSP
033000080527     C                   else
033100080527     C                   if        depspe = vabNSP AND *in65 = *off
033200100413     C   51              add       VABNCL        sav_VABNCL
033300100413     C   51              add       VABPKB        sav_VABPKB
033400100413     C   51              add       VABVLB        sav_VABVLB
033500100720     C   52              z-add     VABNCL        sav_VABNCL
033600100720     C   52              z-add     VABPKB        sav_VABPKB
033700100720     C   52              z-add     VABVLB        sav_VABVLB
033800100720     C   53              add       VABNCL        sav_VABNCL
033900100720     C   53              z-add     VABPKB        sav_VABPKB
034000100720     C   53              z-add     VABVLB        sav_VABVLB
034100080527     C                   else
034200080527     C  N31              eval      fivabds_ok = fivabds_sav
034300080527     C  N31              exsr      wrivab
034400080527     C  N31              eval      fivabds_sav = fivabds
034500080527     C                   eval      depspe = vabNSP
034600080527     C                   endif
034700080527     C                   endif
034800080527     C*
034900080527     C                   ENDSR
035000040119
035100040119
035200040119
035300040119     C*----------------------------------------------------*
035400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035500040119     C*----------------------------------------------------*
035600040510     C     WRIVAB        BEGSR
035700080513     C*
035800080513     C* Gestisco l'eventuale rottura x numero spedizione
035900080527     C                   if        VABNSP <> depspe or *in65 = *on
036000070301     C                   if        wVATNOT_E <> *blanks
036100070301     C                   eval      VABCTM = '7Q'
036200070301     C                   endif
036300100413     C                   write     fivab000
036400080513     C                   endif
036500040119     C*
036600040119     C                   ENDSR
036700040119
036800040119
036900040119
037000040119     C*----------------------------------------------------*
037100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037200040119     C*----------------------------------------------------*
037300040510     C     WRIVAT        BEGSR
037400060512     C*
037500060512     C* Inizializzo i campi chiave
037600060512     C                   eval      VATFGS = VABFGS
037700060512     C                   eval      VATAAS = VABAAS
037800060512     C                   eval      VATCCM = VABCCM
037900070823     C                   eval      VATNRS = VABNRS
038000060512     C                   eval      VATNSP = VABNSP
038100060512     C                   eval      VATLNP = VABLNP
038200040119     C*
038300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038400040510     C                   if        VATTRC = *blanks
038500040510     C                   eval      VATTRC = 'E'
038600040510     C                   endif
038700110318     C                   if        VATNOT <> *blanks
038800040510     C                   write     FIVAT000
038900040119     C                   add       1             §CTROKVT
039000110318     C                   endif
039100060331     C*
039200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039300060331     C                   if        wVATNOT_A <> *blanks
039400060331     C                   eval      VATNOT = wVATNOT_A
039500060331     C                   eval      VATTRC = 'A'
039600060331     C                   write     FIVAT000
039700060331     C                   add       1             §CTROKVT
039800060331     C                   endif
039900060331     C*
040000060331     C                   if        wVATNOT_B <> *blanks
040100060331     C                   eval      VATNOT = wVATNOT_B
040200060331     C                   eval      VATTRC = 'B'
040300060331     C                   write     FIVAT000
040400060331     C                   add       1             §CTROKVT
040500060331     C                   endif
040600060331     C*
040700120320     C*
040800120410     C* i rcd di FIVAT con tipo E sono 1 in quanto è una spedizione monocollo
040900060331     C                   if        wVATNOT_E <> *blanks
041000110201     C                   eval      VATNOT = wVATNOT_E
041100060331     C                   eval      VATTRC = 'E'
041200120411     C*
041300120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
041400120411     C*
041500120411     C     1             DO        VABNCL        wNumCollo         3 0
041600120411     C*** da analisi del POC il progressivo colli è max 3 char e deve ccontiene gli zeri non
041700120411     C*** significativi
041800120411     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
041900120411     C                                %editc(wNumCollo:'X')
042000120410     C                   write     FIVAT000
042100120410     C                   add       1             §CTROKVT
042200120411     C                   ENDDO
042300120320     C*
042400120320     C                   endif
042500120320     C*
042600110927     C*
042700110927     C                   if        wVATNOT_I <> *blanks
042800110927     C                   eval      VATNOT = wVATNOT_I
042900110927     C                   eval      VATTRC = 'I'
043000110927     C                   write     FIVAT000
043100110927     C                   add       1             §CTROKVT
043200110927     C                   endif
043300040119     C*
043400040119     C                   ENDSR
043500040119
043600040119
043700040119
043800040119     C*----------------------------------------------------*
043900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044000040119     C*----------------------------------------------------*
044100040119     C     PREVAT        BEGSR
044200040119     C*
044300040119     C* Compongo il nome del membro da dare al FIVATWWR
044400040119     C                   eval      parmbr = vlrhdl
044500040119     C                   movel     'M'           parmbr
044600040510     C                   eval      parccm = vlrksc
044700040119     C                   eval      paropz = '1'
044800040119     C* Effettuo la chiamata al CLLE preposto
044900040119     C                   call(e)   'TITVVTC'
045000040119     C                   parm                    parccm
045100040119     C                   parm                    parmbr
045200040119     C                   parm                    paropz
045300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045400040119     C                   if        %error
045500040119     C                   movel     '1'           chkcall
045600040119     C                   else
045700040119     C                   movel     '0'           chkcall
045800040119     C                   endif
045900040119     C*
046000040119     C                   ENDSR
046100010601
046200010601
046300010601
046400010330     C*----------------------------------------------------*
046500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046600010330     C*----------------------------------------------------*
046700040119     C     INZVAR        BEGSR
046800080527     C*
046900080527     C                   setoff                                       65
047000010330     C*
047100030822     C* Inizializzo variabili di wrk
047200020204     C                   Z-ADD     *zeros        Num5_0            5 0
047300101018     C                   MOVEL     *blanks       wFlgCAS
047400060331     C                   MOVEL     *blanks       wVATNOT_A
047500060331     C                   MOVEL     *blanks       wVATNOT_B
047600060331     C                   MOVEL     *blanks       wVATNOT_E
047700110927     C                   MOVEL     *blanks       wVATNOT_I
047800120411     C                   MOVEL     *blanks       wNOT
047900120411     C                   MOVEL     *blanks       Data8A
048000030822     C*
048100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048200030822     C                   CLEAR                   SkSplitCSV
048300030822     C                   EVAL      i = 1
048400030822     C                   EVAL      posDa = *zeros
048500030822     C                   EVAL      posA  = *zeros
048600080527     C*
048700080527     C                   CLEAR                   FIVABDS
048800080527     C                   CLEAR                   FIVAT000
048900030822     C*
049000030822     C* Reimposto i valori di default
049100030822     C                   EXSR      DEFCAM
049200030822     C*
049300010330     C                   ENDSR
049400010330     C*----------------------------------------------------*
049500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049600010330     C*----------------------------------------------------*
049700010330     C     DEFCAM        BEGSR
049800030822     C*
049900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050000030715     C* e delimitatore testo.
050100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050400100318     C*
050500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050600100318     C                   EVAL      CharSOS = CharNUM
050700030822     C*
050800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050900100720     C                   SETOFF                                       505152
051000100720     C                   SETOFF                                       53
051100100413     C                   SELECT
051200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051300100720     C                   SETON                                        50
051400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051500100413     C                   SETON                                        51
051600100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051700100720     C                   SETON                                        52
051800100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051900100720     C                   SETON                                        53
052000100413     C                   ENDSL
052100030822     C*
052200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052300030822     C                   EVAL      posDaDft = 1
052400030822     C                   EVAL      posADft  = 0
052500030822     C                   EVAL      wGiroDft = 0
052600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052700030822     C                             posDaDft > 0
052800030822     C*
052900030822     C* Gestisco il 1° giro
053000030822     C                   IF        wGiroDft = 0
053100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053300030822     C* Incremento il contatore dei "giri"
053400030822     C                   EVAL      wGiroDft = 1
053500030822     C                   ELSE
053600030822     C                   EVAL      posDaDft = posADft
053700030822     C                   ENDIF
053800030822     C* Eseguo lo scan x trovare la fine del campo corrente
053900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054000030822     C*
054100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054300030822     C                   IF        posDaDft > 0 AND
054400030822     C                             posADft  > 0
054500030822     C* NCL
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'NCL'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABNCL
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* CCM
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'CCM'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiInt=*on
056800030822     C                   Z-ADD     PiVal         VABCCM
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* LNP
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'LNP'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiInt=*on
058100030822     C                   Z-ADD     PiVal         VABLNP
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* NRS
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'NRS'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiInt=*on
059400030822     C                   Z-ADD     PiVal         VABNRS
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* CTR
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'CTR'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABCTR
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* PKB
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'PKB'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiNum=*on
062000120418     C                   Z-ADD     PiVal         VABPKB
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* VLB
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'VLB'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiNum=*on
063300030822     C                   Z-ADD     PiVal         VABVLB
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* QFT
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'QFT'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiNum=*on
064600030822     C                   Z-ADD     PiVal         VABQFT
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* CBO
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'CBO'
065400030822     C                   EVAL      VABCBO=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* TSP
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'TSP'
066300030822     C                   EVAL      VABTSP=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* VAS
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VAS'
067200030822     C                   EVAL      VABVAS=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* VCA
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VCA'
068100030822     C                   EVAL      VABVCA=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500070925     C* TIC
068600070925     C                   IF        %subst(
068700070925     C                             %subst(vlrppt:posDaDft+1:
068800070925     C                             posADft-posDaDft-1):1:3)
068900070925     C                             = 'TIC'
069000070925     C                   EVAL      VABTIC=%trim(%subst(
069100070925     C                             %subst(vlrppt:posDaDft+1:
069200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300070925     C                   ENDIF
069400030822     C* GCA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'GCA'
069900030822     C                   EVAL      VABGCA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* CTM
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'CTM'
070800030822     C                   EVAL      VABCTM=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* FFD
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'FFD'
071700030822     C                   EVAL      VABFFD=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* VAD
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'VAD'
072600030822     C                   EVAL      VABVAD=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* GMA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'GMA'
073500030822     C                   EVAL      VABGMA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GGA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GGA'
074400030822     C                   EVAL      VABGGA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* GVA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GVA'
075300030822     C                   EVAL      VABGVA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* TC1
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'TC1'
076200030822     C                   EVAL      VABTC1=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* TC2
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'TC2'
077100030822     C                   EVAL      VABTC2=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500040714     C* VATTRC
077600040714     C                   IF        %subst(
077700040714     C                             %subst(vlrppt:posDaDft+1:
077800040714     C                             posADft-posDaDft-1):1:3)
077900040714     C                             = 'TRC'
078000040714     C                   EVAL      VATTRC=%trim(%subst(
078100040714     C                             %subst(vlrppt:posDaDft+1:
078200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300040714     C                   ENDIF
078400030822     C* ...
078500030822     C                   ENDIF
078600030822     C                   ENDDO
078700020204     C*
078800010330     C                   ENDSR
078900010607     C*----------------------------------------------------*
079000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079100010607     C*----------------------------------------------------*
079200010607     C     IMPVAB        BEGSR
079300010607     C*
079400010607     C                   SETOFF                                       3132
079500110315 xxx C***                SETON                                        35
079600030515     C*
079700010607     C                   MOVEL     datcor        VABAAS
079800010607     C                   MOVE      datcor        VABMGS
079900010607     C*
080000030715     C**********
080100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080200030715     C**********
080300030715     C                   IF        wGiro = *zeros
080400040113     C*
080500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080800080429     C                             %trim(intcol(5))<>*blanks
080900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081100080429     C                                      %trim(intcol(5))
081200070823     C     ';':CharCSV   XLATE     vindta        vindta
081300040510     C                   ENDIF
081400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081500031201     C     minu:maiu     XLATE     vindta        vindta
081600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081700030715     C                   DOW       posDa <= %len(%trim(vindta))
081800030715     C*
081900030715     C* Gestisco il 1° campo
082000030715     C                   IF        i = 1
082100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082200030715     C                   EVAL      posDa = 1
082300030715     C* Eseguo lo scan x trovare la fine del primo campo
082400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082500030715     C* A questo punto "estraggo" il campo corrente
082600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082700030715     C                                                    (posA-posDa))
082800030715     C* X i campi successivi al 1°
082900030715     C                   ELSE
083000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083100030715     C                   EVAL      posDa = posA + 1
083200030715     C* Eseguo lo scan x trovare la fine del campo corrente
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* Gestisco l'ultimo campo
083500030715     C                   IF        posA = *zeros
083600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   LEAVE
084100030715     C                   ELSE
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   ENDIF
084600030715     C                   ENDIF
084700030715     C* Incremento il contatore d campo
084800030715     C                   EVAL      i = i +1
084900030715     C                   ENDDO
085000030715     C*
085100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085200030715     C* delimitazione testo
085300030715     C                   EVAL      i = 1
085400030715     C                   DOW       i < %elem(SkSplitFLD)
085500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085700030715     C                   EVAL      i = i + 1
085800030715     C                   ENDDO
085900030715     C                   EVAL      wGiro = 1
086000030715     C                   ELSE
086100030715     C                   EVAL      wGiro = 2
086200031204     C**********
086300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086400031204     C* d separatore campo
086500031204     C**********
086600031204     C                   EVAL      vindta = %trim(vindta)
086700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086800031204     C                   EVAL      vindta = %subst(vindta:2)
086900031204     C                   ENDDO
087000031204     C*
087100031204     C                   Z-ADD     *zeros        lunghInput        4 0
087200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087600110912     C                   ENDDO
087700030715     C**********
087800030715     C* Effettuo lo split del campo dati d input
087900030715     C**********
088000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088100030715     C                   DOW       posDa <= %len(%trim(vindta))
088200030715     C*
088300030715     C* Gestisco il 1° campo
088400030715     C                   IF        i = 1
088500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088600030715     C                   EVAL      posDa = 1
088700030715     C* Eseguo lo scan x trovare la fine del primo campo
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C* X i campi successivi al 1°
089300030715     C                   ELSE
089400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089500030715     C                   EVAL      posDa = posA + 1
089600031204     C* Verifico che nn vi sia il campo nn valorizzato
089700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089800031204     C* Se campo nn valorizzato skippo al prossimo
089900031204     C                   EVAL      SkSplitCSV(i) = *blanks
090000031204     C                   EVAL      posA  = posA + 1
090100031204     C                   ELSE
090200030715     C* Eseguo lo scan x trovare la fine del campo corrente
090300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090400030715     C* Gestisco l'ultimo campo
090500030715     C                   IF        posA = *zeros
090600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C                   LEAVE
091100030715     C                   ELSE
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C                   ENDIF
091600031204     C                   ENDIF
091700030715     C                   ENDIF
091800030715     C* Incremento il contatore d campo
091900030715     C                   EVAL      i = i +1
092000030715     C                   ENDDO
092100030715     C*
092200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092300030715     C* delimitazione testo
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitCSV)
092600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092800030715     C                   EVAL      i = i + 1
092900030715     C                   ENDDO
093000100720     C*
093100100720     C* Stacco un progressivo x numero spedizione
093200120301 xxx C                   EXSR      repNSP
093300100326     C*
093400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093500030715     C                   EVAL      i = 1
093600030715     C                   DOW       i < %elem(SkSplitFLD)
093700030715     C*
093800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094000030715     C                             SkSplitCSV(i) <> *blanks
094100030721     C*** ATB
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094300030721     C                   EVAL      VABATB = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030715     C*** RSD
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095000030715     C* ==
095100030715     C                   ENDIF
095200030721     C*** RD2
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030715     C*** IND
095700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095800030715     C                   EVAL      VABIND = SkSplitCSV(i)
095900030715     C                   ENDIF
096000030715     C*** LOD
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096300030715     C                   ENDIF
096400030715     C*** PRD
096500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096700030715     C                   ENDIF
096800030721     C*** NZD
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030715     C*** RMA
097300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097500030715     C                   ENDIF
097600030715     C*** NOT
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** NT2
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** VCA
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** VAS
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** FFD
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030721     C*** GC1
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** GC2
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** TSP
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100700030721     C                   ENDIF
100800040510     C*** CBO
100900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101100040510     C                   ENDIF
101200030721     C*** NAS
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** TIC
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** GCA
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** XCO
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** CTM
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** TCR
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** CTS
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** FTM
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** VAD
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** GMA
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** GGA
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** GVA
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** TC1
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TC2
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** SCL
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** RMO
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** NMO
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*
108100030721     C* Reperisco quindi i campi numerici...
108200040510     C*** NSP
108300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108400040510     C                   EVAL      PiStr=SkSplitCSV(i)
108500040510     C                   EXSR      CHKNUM
108600040510     C                   IF        PiInt=*on
108700040510     C                   Z-ADD     PiVal         VABNSP
108800040510     C                   ELSE
108900040510     C                   SETON                                        31
109000040510     C                   EVAL      VABNSP = *zeros
109100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109200040510     C                             + ' ' + 'VABNSP'
109300040510     C                   ENDIF
109400040510     C                   ENDIF
109500071106     C*** RMN
109600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109700071106     C                   EVAL      PiStr=SkSplitCSV(i)
109800071106     C                   EXSR      CHKNUM
109900071106     C                   IF        PiInt=*on
110000071106     C                   Z-ADD     PiVal         VABRMN
110100071106     C                   ELSE
110200071106     C                   SETON                                        32
110300071106     C                   EVAL      VABRMN = *zeros
110400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110500071106     C                             + ' ' + 'VABRMN'
110600071106     C                   ENDIF
110700071106     C                   ENDIF
110800030721     C*** CAD
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiInt=*on
111300030721     C                   Z-ADD     PiVal         Num5_0
111400030721     C                   MOVEL(p)  Num5_0        VABCAD
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      VABCAD = *zeros
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABCAD'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** NCL
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on
112700030721     C                   Z-ADD     PiVal         VABNCL
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000030721     C                   EVAL      VABNCL = *zeros
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200040122     C                             + ' ' + 'VABNCL'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** PKB
113600120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
113700120418     C* per cui si arrotonda il valore passato agli etti
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiNum=*on
114200120418     C                   Z-ADD(H)  PiVal         VABPKB
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABPKB = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABPKB'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** CAS
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115300111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115400111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
115500120227 xxx C                   EVAL      VABvca = 'EUR'
115600101018     C                   EVAL      wFlgCAS = '1'
115700101018     C                   ENDIF
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiNum=*on
116100030721     C                   Z-ADD     PiVal         VABCAS
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABCAS = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABCAS'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** CCM
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on      AND
117400030721     C                             PiVal<=9999999 AND
117500030721     C                             PiVal>*zeros
117600030721     C                   Z-ADD     PiVal         VABCCM
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABCCM = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100040510     C                             + ' ' + 'VABCCM'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** LNP
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on    AND
118900030721     C                             PiVal<=999   AND
119000030721     C                             PiVal>*zeros
119100030721     C                   Z-ADD     PiVal         VABLNP
119200030721     C                   ELSE
119300040715     C                   SETON                                        32
119400030721     C                   EVAL      VABLNP = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600040510     C                             + ' ' + 'VABLNP'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** NRS
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on AND
120400030721     C                             PiVal<=99
120500030721     C                   Z-ADD     PiVal         VABNRS
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABNRS'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** LNA
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on    AND
121700030721     C                             PiVal<=999   AND
121800030721     C                             PiVal>*zeros
121900030721     C                   Z-ADD     PiVal         VABLNA
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      VABLNA = *zeros
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABLNA'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** CTR
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on  AND
123200030721     C                             PiVal<=999
123300030721     C                   Z-ADD     PiVal         VABCTR
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABCTR'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** IAS
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiNum=*on
124500030721     C                   Z-ADD     PiVal         VABIAS
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABIAS'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** VLB
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABVLB
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABVLB'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** QFT
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABQFT
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABQFT'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** NCD
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on      AND
128100030721     C                             PiVal<=9999999
128200030721     C                   Z-ADD     PiVal         VABNCD
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABNCD'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** NCA
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on      AND
129400030721     C                             PiVal<=9999999
129500030721     C                   Z-ADD     PiVal         VABNCA
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABNCA'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** ZNC
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on AND
130700030721     C                             PiVal<=99
130800030721     C                   Z-ADD     PiVal         VABZNC
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABZNC'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** DCR
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on       AND
132000030721     C                             PiVal<=99999999
132100030721     C                   Z-ADD     PiVal         VABDCR
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABDCR'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** HCR
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on   AND
133300030721     C                             PiVal<=9999
133400030721     C                   Z-ADD     PiVal         VABHCR
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABHCR'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** VMD
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiNum=*on
134600030721     C                   Z-ADD     PiVal         VABVMD
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABVMD'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** ANT
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on        AND
135800030721     C                             PiVal<=999999999
135900030721     C                   Z-ADD     PiVal         VABANT
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABANT'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** CMO
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on
137100030721     C                   Z-ADD     PiVal         Num5_0
137200030721     C                   MOVEL(p)  Num5_0        VABCMO
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      VABCMO = *zeros
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABCMO'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000040510     C*
138100040510     C* Reperisco i campi del FIVAT
138200040510     C*** VATNOT
138300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138500040510     C                   ENDIF
138600040510     C*** VATTRC
138700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138900040510     C                   ENDIF
139000060331     C*** VATNOT_A
139100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139300060331     C                   ENDIF
139400060331     C*** VATNOT_B
139500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139700060331     C                   ENDIF
139800060331     C*** VATNOT_E
139900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140100060331     C                   ENDIF
140200110927     C*** VATNOT_I
140300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140500110927     C                   ENDIF
140600070823     C*
140700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140800120301
140900120418      *** W_VABRSD = VABRSD + VABRD2
141000120418     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141100120418     C                   MOVEL     *blanks       wRSD             70
141200120418     C                   EVAL      wRSD=%trim(SkSplitCSV(i))
141300120418     C                   EVAL      VABRSD=%subst(wRSD:1:35)
141400120418     C                   EVAL      VABRD2=%subst(wRSD:36:35)
141500120418     C                   ENDIF
141600120418      *** W_VABNOT = VABNOT + VABNT2
141700120418     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
141800120418     C                   MOVEL     *blanks       wNOT             70
141900120418     C                   EVAL      wNOT=%trim(SkSplitCSV(i))
142000120418     C                   EVAL      VABNOT=%subst(wNOT:1:35)
142100120418     C                   EVAL      VABNT2=%subst(wNOT:36:35)
142200120418     C                   ENDIF
142300070925     C*
142400071029     C***  <===  -----------------------------------------
142500040510     C*
142600030715     C                   ENDIF
142700030715     C                   EVAL      i = i + 1
142800030715     C                   ENDDO
142900070925     C*
143000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
143100120411     C*
143200120411     C* wNOT è stato valorizzato concatenando più colonne
143300120411     C                   EVAL      VABNOT=%subst(wNOT:1:35)
143400120411     C                   EVAL      VABNT2=%subst(wNOT:36:35)
143500100831     C*
143600070925     C*
143700071029     C***  <===  -----------------------------------------
143800070925     C*
143900070925     C* Considerazioni finali su CBO/CAS
144000100318     C                   IF        wFlgCAS = '1'    OR
144100100318     C                             VABCAS  > *zeros
144200070925     C                   IF        VABCBO = '1'
144300070925     C                   EVAL      VABCBO = '4'
144400070925     C                   ENDIF
144500070925     C                   IF        VABCBO = '2'
144600070925     C                   EVAL      VABCBO = '6'
144700070925     C                   ENDIF
144800070925     C                   ENDIF
144900070925     C***  <===  ----------------------------
145000040510     C*
145100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145200040510     C                   IF        VABNSP = *zeros
145300040510     C                   SETON                                        31
145400040510     C                   ENDIF
145500040715     C*
145600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
145700040715     C                   IF        VABLNP = *zeros
145800040715     C                   SETON                                        31
145900040715     C                   ENDIF
146000040510     C*
146100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146200120411     C                   IF        VABRMN = *zeros
146300120411     C                   Z-ADD     VABNSP        VABRMN
146400120411     C                   ENDIF
146500120328     C* IN QUESTO CASO LO FORZO = 1
146600120411     C***                IF        VABRMN = *zeros
146700120411     C***                Z-ADD     1             VABRMN
146800120411     C***                ENDIF
146900040510     C*
147000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147100040510     C                   IF        VABRMA = *blanks
147200040714     C                   MOVEL     VABRMN        VABRMA
147300040510     C                   ENDIF
147400040128     C*
147500040510     C* Se provincia nn valorizzata la reperisco
147600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
147700110912     C                   IF        VABPRD =  *blanks AND
147800110912     C                             VABNZD =  *blanks AND
147900110912     C                             VABCAD <> *blanks AND
148000110912     C                             VABLOD <> *blanks
148100040128     C                   CLEAR                   TISI95DS
148200040128     C                   EVAL      I95TCN = '3'
148300040128     C                   Z-ADD     datcor        I95DAT
148400040128     C                   EVAL      I95NAR = VABNZD
148500040128     C                   EVAL      I95CAP = VABCAD
148600040128     C                   EVAL      I95LOC = VABLOD
148700040128     C                   CALL      'TISI95R'
148800040128     C                   PARM                    TISI95DS
148900040128     C                   EVAL      VABPRD = O95PRV
149000040510     C                   ENDIF
149100020204     C*
149200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149300020204     C                   EXSR      CHKIMPDIV
149400030715     C                   ENDIF
149500020204     C*
149600010607     C                   ENDSR
149700010607     C*----------------------------------------------------*
149800010601
149900020204
150000020204     C*----------------------------------------------------*
150100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150200020204     C*----------------------------------------------------*
150300020204     C     CHKIMPDIV     BEGSR
150400020204     C*
150500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150600020204     C                   Z-ADD     *zeros        wrkDec            9 9
150700020204     C*
150800020204     C* Come prima cosa effettuo considerazioni sulla divisa
150900020204     C                   IF        vabIAS > *zeros
151000020204     C                   IF        vabVAS <> 'EUR'
151100020204     C                   EVAL      vabVAS =  'ITL'
151200020204     C                   ENDIF
151300020204     C                   ENDIF
151400020204     C*
151500020204     C                   IF        vabCAS > *zeros
151600020204     C                   IF        vabVCA <> 'EUR'
151700020204     C                   EVAL      vabVCA =  'ITL'
151800020204     C                   ENDIF
151900020204     C                   ENDIF
152000020204     C*
152100020204     C                   IF        vabVMD > *zeros
152200020204     C                   IF        vabVAD <> 'EUR'
152300020204     C                   EVAL      vabVAD =  'ITL'
152400020204     C                   ENDIF
152500020204     C                   ENDIF
152600020204     C*
152700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152800020204     C                   Z-ADD     vabIAS        wrkDec
152900020204     C                   IF        wrkDec > *zeros
153000020204     C                   IF        vabVAS = 'ITL'
153100020204     C                   EVAL      vabIAS = *zeros
153200020204     C                   ENDIF
153300020204     C                   ENDIF
153400020204     C*
153500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153600020204     C                   Z-ADD     vabCAS        wrkDec
153700020204     C                   IF        wrkDec > *zeros
153800020204     C                   IF        vabVCA = 'ITL'
153900020204     C                   EVAL      vabCAS = *zeros
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154400020204     C                   Z-ADD     vabVMD        wrkDec
154500020204     C                   IF        wrkDec > *zeros
154600020204     C                   IF        vabVAD = 'ITL'
154700020204     C                   EVAL      vabVMD = *zeros
154800020204     C                   ENDIF
154900020204     C                   ENDIF
155000020204     C*
155100020204     C                   ENDSR
155200020204     C***
155300010330
155400100318
155500100318
155600100318     C*----------------------------------------------------*
155700100318     C*  CONTROLLO NUMERICITA' CAMPI
155800100318     C*----------------------------------------------------*
155900100318     C     CHKNUM        BEGSR
156000100318     C*
156100100318     C                   IF        PiDecChr = *blanks
156200100318     C                   EVAL      PiDecChr = CharNUM
156300100318     C                   ENDIF
156400100318     C*
156500100318     C                   callp     UBISNUM_Check(PiStr
156600100318     C                                          :PiDecChr
156700100318     C                                          :PiVal
156800100318     C                                          :PiNum
156900100318     C                                          :PiInt)
157000100318     C*
157100100318     C                   ENDSR
157200100318     C***
157300010601
157400010601
157500010601
157600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157700010601     C     repfil        BEGSR
157800010601     C*
157900010601     C                   if        invfil = *zeros and
158000010601     C                             depfil > *zeros and
158100010629     C                             (vinflg = *blanks or
158200010629     C                              vinflg = *zeros)
158300010601     C
158400010601     C                   eval      invfil = depfil
158500010601     C                   endif
158600010601     C*
158700010601     C                   if        depfil <> invfil and
158800010601     C                             invfil > *zeros
158900010601     C                   eval      flgMulti = '1'
159000010601     C                   if        vinflg = *blanks
159100010601     C                   add       1             cntNonEl
159200010601     C                   endif
159300010601     C                   endif
159400010601     C*
159500010601     C                   if        vinflg = '2'
159600010601     C                   eval      flgStato = '2'
159700010601     C                   endif
159800010601     C*
159900010601     C                   ENDSR
160000010601     C***
160100010601
160200010601
160300010601
160400010330
160500040119      /TITLE Invio dei dati al punto operativo.
160600040119     C     invio         BEGSR
160700040119     C*
160800040119     C* 1° invio FIVAT
160900040119     C                   reset                   dscmz
161000040119     C                   move      vlrpoi        cmzdst
161100040119     C                   eval      cmzfld = 'FIVATWWR'
161200040119     C                   eval      cmzmbd = vlrhdl
161300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161400040119     C***                if        prmfir = *blanks
161500040119     C                   eval      cmzfla = 'FIVAT00F'
161600040119     C                   eval      cmzmba = 'FIVAT00F'
161700040119     C***                else
161800040119     C***                eval      cmzfla = prmfir
161900040119     C***                eval      cmzmba = prmfir
162000040119     C***                endif
162100040119     C                   eval      cmznrr = *zeros
162200040119     C                   move      §ctrokvt      cmznrr
162300040119     C                   eval      cmzlba = vlrfl1
162400040119     C                   call(e)   'TIS711C'
162500040119     C                   parm                    dscmz
162600040119     C                   parm      *blanks       esito
162700040119     C                   if        %error
162800040119     C                             or cmzerr = '1'
162900040119     C                             or esito  = '1'
163000040119     C                   eval      wrkesito = '3'
163100040119     C                   else
163200040119     C*
163300040119     C* 2° invio FIVAB
163400040119     C                   reset                   dscmz
163500040119     C                   move      vlrpoi        cmzdst
163600040119     C                   eval      cmzfld = vlrfou
163700040119     C                   eval      cmzmbd = vlrhdl
163800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163900040119     C***                if        prmfir = *blanks
164000040119     C                   eval      cmzfla = 'FIVAB00F'
164100040119     C                   eval      cmzmba = 'FIVAB00F'
164200040119     C***                else
164300040119     C***                eval      cmzfla = prmfir
164400040119     C***                eval      cmzmba = prmfir
164500040119     C***                endif
164600040119     C                   eval      cmznrr = *zeros
164700040119     C                   move      §ctrokvb      cmznrr
164800040119     C                   eval      cmzlba = vlrfl1
164900040119     C                   call(e)   'TIS711C'
165000040119     C                   parm                    dscmz
165100040119     C                   parm      *blanks       esito
165200040119     C                   if        %error
165300040119     C                             or cmzerr = '1'
165400040119     C                             or esito  = '1'
165500040119     C                   eval      wrkesito = '3'
165600040119     C                   endif
165700040119     C                   endif
165800040119     C*
165900040119     C                   ENDSR
166000040119     C***
166100010601
166200010601
166300010601
166400010601
166500010601      /TITLE Invio dei dati al punto operativo.
166600010601     C     opeini        BEGSR
166700010601     C*
166800010601     C* Inizializzo flag e contatori operativi
166900010601     C                   movel     '0'           flgGiro           1
167000010601     C                   movel     '0'           flgMulti          1
167100010601     C                   movel     '1'           flgStato          1
167200010615     C                   movel     '0'           flgOk             1
167300010601     C                   z-add     *zeros        cntNonEl         10 0
167400010601     C                   z-add     *zeros        depfil            3 0
167500010601     C                   z-add     *zeros        invfil            3 0
167600010601     C*
167700010601     C                   ENDSR
167800010601     C***
167900070326
168000070326
168100070326
168200070326
168300070326     C     *pssr         BEGSR
168400070329     C*
168500070329     C                   if        %open(tivin00r)
168600070329     C                   close     tivin00r
168700070329     C                   endif
168800070329     C                   if        %open(fivabwwr)
168900070329     C                   close     fivabwwr
169000070329     C                   endif
169100070329     C                   if        %open(fivatwwr)
169200070329     C                   close     fivatwwr
169300070329     C                   endif
169400070326     C*
169500070326     C* Effettuo la chiamata al CLLE preposto
169600070326     C                   call(e)   'TITVVTC'
169700070326     C                   parm                    parccm
169800070326     C                   parm                    parmbr
169900070326     C                   parm      '2'           paropz
170000070326     C*
170100070326     C                   eval      wrkesito = '2'
170200070404     C*
170300070404     C                   seton                                        LR
170400070326     C*
170500070326     C                   ENDSR     '*CANCL'
170600070326     C***
170700100318
170800100318
170900100318
171000100318     C     normCSV       BEGSR
171100100318     C*
171200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
171300100318     C* separatori campi
171400100318     C                   if        CharTXT <> *blanks
171500100318     C                   z-add     *zeros        wLenVINDTA        4 0
171600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
171700100318     C                   setoff                                       80
171800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
171900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
172000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
172100100318     C                   if        not *in80
172200100318     C                   seton                                        80
172300100318     C                   else
172400100318     C                   setoff                                       80
172500100318     C                   endif
172600100318     C                   endif
172700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
172800100318     C                             and *in80 = *on
172900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
173000100318     C                   endif
173100100318     C                   enddo
173200100318     C                   endif
173300100318     C*
173400100318     C                   ENDSR
173500100318     C***
173600070326
173700100326
173800100326
173900100326     C*----------------------------------------------------*
174000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
174100100326     C*----------------------------------------------------*
174200100326     C     REPNSP        BEGSR
174300100326     C*
174400100326     C* NSP => Stacco un numeratore da AZNUM
174500100326     C                   clear                   TRUL33DS
174600100326     C                   eval      I33OPE = *zeros
174700100326     C                   eval      I33CNU = 302
174800100326     C                   eval      I33NUM = 1
174900100326     C                   movel     TRUL33DS      KPJBU
175000100326     C                   call      'TRUL33R'
175100100326     C                   parm                    KPJBA
175200100326     C                   movel     KPJBU         TRUL33DS
175300100326     C                   if        O33ERR = *zeros
175400100326     C                   z-add     O33NRF        VABNSP
175500100326     C                   z-add     O33NRF        VATNSP
175600100326     C                   else
175700100326     C                   SETON                                        31
175800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
175900100326     C                             + ' ' + 'VABNSP VATNSP'
176000100326     C                   endif
176100100326     C*
176200100326     C                   ENDSR
176300010330
176400100326
176500010330
176600000613     C     *inzsr        BEGSR
176700990910     C*
176800990910     C     *entry        plist
176900990920     C                   parm                    tivlrds
177000990921     C                   parm      wrkesito      esito
177100000724     C                   parm                    prmlit
177200000710     C                   parm                    prmfir
177300010330     C*
177400010330     C* CALCOLA LA DATA CORRENTE
177500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
177600100318     C                   eval      datcor = %dec(%date() : *ISO)
177700000613     C*
177800000613     C                   ENDSR
177900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178000120621W_VABRSD;VABIND;VABPRD;VABCAD;VABLOD;VABNCL;VABPKB;VABCAS;W_VABNOT;VATNOT_I
178100120418
178200120418
178300111116
178400100318
