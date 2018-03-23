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
003600120411     D Intestazioni    s               n   inz(*on)
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
062000030822     C                   Z-ADD     PiVal         VABPKB
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
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiNum=*on
114000030721     C                   Z-ADD     PiVal         VABPKB
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      VABPKB = *zeros
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABPKB'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** CAS
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115000110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115100111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115200111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
115300120227 xxx C                   EVAL      VABvca = 'EUR'
115400101018     C                   EVAL      wFlgCAS = '1'
115500101018     C                   ENDIF
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiNum=*on
115900030721     C                   Z-ADD     PiVal         VABCAS
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      VABCAS = *zeros
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABCAS'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** CCM
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on      AND
117200030721     C                             PiVal<=9999999 AND
117300030721     C                             PiVal>*zeros
117400030721     C                   Z-ADD     PiVal         VABCCM
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      VABCCM = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900040510     C                             + ' ' + 'VABCCM'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** LNP
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiInt=*on    AND
118700030721     C                             PiVal<=999   AND
118800030721     C                             PiVal>*zeros
118900030721     C                   Z-ADD     PiVal         VABLNP
119000030721     C                   ELSE
119100040715     C                   SETON                                        32
119200030721     C                   EVAL      VABLNP = *zeros
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400040510     C                             + ' ' + 'VABLNP'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** NRS
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on AND
120200030721     C                             PiVal<=99
120300030721     C                   Z-ADD     PiVal         VABNRS
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABNRS'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** LNA
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on    AND
121500030721     C                             PiVal<=999   AND
121600030721     C                             PiVal>*zeros
121700030721     C                   Z-ADD     PiVal         VABLNA
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      VABLNA = *zeros
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABLNA'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** CTR
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on  AND
123000030721     C                             PiVal<=999
123100030721     C                   Z-ADD     PiVal         VABCTR
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABCTR'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** IAS
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABIAS
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABIAS'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** VLB
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiNum=*on
125500030721     C                   Z-ADD     PiVal         VABVLB
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABVLB'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** QFT
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiNum=*on
126700030721     C                   Z-ADD     PiVal         VABQFT
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABQFT'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** NCD
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on      AND
127900030721     C                             PiVal<=9999999
128000030721     C                   Z-ADD     PiVal         VABNCD
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABNCD'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** NCA
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiInt=*on      AND
129200030721     C                             PiVal<=9999999
129300030721     C                   Z-ADD     PiVal         VABNCA
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABNCA'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** ZNC
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on AND
130500030721     C                             PiVal<=99
130600030721     C                   Z-ADD     PiVal         VABZNC
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABZNC'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** DCR
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiInt=*on       AND
131800030721     C                             PiVal<=99999999
131900030721     C                   Z-ADD     PiVal         VABDCR
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132300030721     C                             + ' ' + 'VABDCR'
132400030721     C                   ENDIF
132500030721     C                   ENDIF
132600030721     C*** HCR
132700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132800030721     C                   EVAL      PiStr=SkSplitCSV(i)
132900030721     C                   EXSR      CHKNUM
133000030721     C                   IF        PiInt=*on   AND
133100030721     C                             PiVal<=9999
133200030721     C                   Z-ADD     PiVal         VABHCR
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABHCR'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900030721     C*** VMD
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiNum=*on
134400030721     C                   Z-ADD     PiVal         VABVMD
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABVMD'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** ANT
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on        AND
135600030721     C                             PiVal<=999999999
135700030721     C                   Z-ADD     PiVal         VABANT
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABANT'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** CMO
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on
136900030721     C                   Z-ADD     PiVal         Num5_0
137000030721     C                   MOVEL(p)  Num5_0        VABCMO
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      VABCMO = *zeros
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABCMO'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800040510     C*
137900040510     C* Reperisco i campi del FIVAT
138000040510     C*** VATNOT
138100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138300040510     C                   ENDIF
138400040510     C*** VATTRC
138500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138700040510     C                   ENDIF
138800060331     C*** VATNOT_A
138900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139100060331     C                   ENDIF
139200060331     C*** VATNOT_B
139300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139500060331     C                   ENDIF
139600060331     C*** VATNOT_E
139700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139900060331     C                   ENDIF
140000110927     C*** VATNOT_I
140100110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140200110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140300110927     C                   ENDIF
140400070823     C*
140500100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140600120301
140700120411      *** W_VABTCR: mandano D=Dopo (come noi), P=Prima (come noi), I=Il ma noi usiamo ' '
140800120411     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTCR'
140900120411     C                   SELECT
141000120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'D'
141100120411     C                   EVAL      VABTCR='D'
141200120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'P'
141300120411     C                   EVAL      VABTCR='P'
141400120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'I'
141500120411     C                   EVAL      VABTCR=*blank
141600120411     C                   ENDSL
141700120411     C                   ENDIF
141800120411      *** W_VABDCR: la data è in formato gg/mm/aaaa alfanum. e va trasformata in 8 S
141900120411     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABDCR'
142000120517     C* il cliente può mandare la data con l'anno di 2 o di 4, bisogna gestirla
142100120517     C                   IF        %len(%trim(SkSplitCSV(i)))=10
142200120411     C                   EVAL      Data8A = %subst(%trim(SkSplitCSV(i)):7:4) +
142300120411     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
142400120411     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
142500120411     C                   MOVEL     Data8A        VABDCR
142600120517     C                   ELSE
142700120517     C* se la data non fosse né di 6 né di 8, valorizzo VABDCR come se mi arrivasse di 6
142800120517     C* così se il valore è impossibile va in errore e ce ne accorgiamo
142900120517     C                   EVAL      Data8A = %subst(%char(datcor):1:2) +
143000120517     C                                      %subst(%trim(SkSplitCSV(i)):7:2) +
143100120517     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
143200120517     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
143300120517     C                   MOVEL     Data8A        VABDCR
143400120517     C                   ENDIF
143500120517     C                   ENDIF
143600120411      *** W_VABCBO: F = 1, A = 2
143700120411     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
143800120411     C                   SELECT
143900120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'F'
144000120411     C                   EVAL      VABCBO='1'
144100120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'A'
144200120411     C                   EVAL      VABCBO='2'
144300120411     C* se sbagliato valore imposto di default Porto Franco
144400120411     C                   OTHER
144500120411     C                   EVAL      VABCBO='1'
144600120411     C                   ENDSL
144700120411     C                   ENDIF
144800120411      *** wNOT è la concatenazione di W_VABNOT e WVABNT2_1/2/3
144900120411      *** ci sono 4 colonne nel .csv che si riferiscono a note di spedizione
145000120411      *** vengono tutte concatenate e alla fine suddivise tra VABNOT e VABNT2
145100120411     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT' or
145200120411     C                             %trim(SkSplitFLD(i)) = 'W_VABNT2_1' or
145300120411     C                             %trim(SkSplitFLD(i)) = 'W_VABNT2_2' or
145400120411     C                             %trim(SkSplitFLD(i)) = 'W_VABNT2_3'
145500120411     C                   EVAL      wNOT=%trim(wNOT)+' '+%trim(SkSplitCSV(i))
145600120411     C                   ENDIF
145700070925     C*
145800071029     C***  <===  -----------------------------------------
145900040510     C*
146000030715     C                   ENDIF
146100030715     C                   EVAL      i = i + 1
146200030715     C                   ENDDO
146300070925     C*
146400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146500120411     C*
146600120411     C* wNOT è stato valorizzato concatenando più colonne
146700120411     C                   EVAL      VABNOT=%subst(wNOT:1:35)
146800120411     C                   EVAL      VABNT2=%subst(wNOT:36:35)
146900100831     C*
147000070925     C*
147100071029     C***  <===  -----------------------------------------
147200070925     C*
147300070925     C* Considerazioni finali su CBO/CAS
147400100318     C                   IF        wFlgCAS = '1'    OR
147500100318     C                             VABCAS  > *zeros
147600130510     C                   EVAL      VABTIC = 'TM'
147700070925     C                   IF        VABCBO = '1'
147800070925     C                   EVAL      VABCBO = '4'
147900070925     C                   ENDIF
148000070925     C                   IF        VABCBO = '2'
148100070925     C                   EVAL      VABCBO = '6'
148200070925     C                   ENDIF
148300070925     C                   ENDIF
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
179100100326     C                   else
179200100326     C                   SETON                                        31
179300100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179400100326     C                             + ' ' + 'VABNSP VATNSP'
179500100326     C                   endif
179600100326     C*
179700100326     C                   ENDSR
179800010330
179900100326
180000010330
180100000613     C     *inzsr        BEGSR
180200990910     C*
180300990910     C     *entry        plist
180400990920     C                   parm                    tivlrds
180500990921     C                   parm      wrkesito      esito
180600000724     C                   parm                    prmlit
180700000710     C                   parm                    prmfir
180800010330     C*
180900010330     C* CALCOLA LA DATA CORRENTE
181000100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181100100318     C                   eval      datcor = %dec(%date() : *ISO)
181200000613     C*
181300000613     C                   ENDSR
181400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181500130510VABRMO;X;VATNOT_E;VABNCL;X;VABRSD;VABRD2;VABIND;X;VABLOD;VABCAD;VABPRD;VATNOT_A;VATNOT_B;
181600130510W_VABTCR;W_VABDCR;W_VABNOT;VABNSP;VABCCM;W_VABCBO;X;X;X;X;X;X;VABPKB;X;VABIAS;X;VABCAS;
181700120411W_VABNT2_1;W_VABNT2_2;W_VABNT2_3
181800111116
181900100318
