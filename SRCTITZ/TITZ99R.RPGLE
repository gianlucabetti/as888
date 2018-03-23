000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400130618      * Disk A
000500130625      *
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
004100130618     D Intestazioni    s               n   inz(*on)
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
007000120411     D wNOT            s             70
007100120411     D Data8A          s              8
007200030822     D*------------------
007300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007400030822     D*------------------
007500030822     D posDaDft        S              3  0 INZ(*zeros)
007600030822     D posADft         S              3  0 INZ(*zeros)
007700030822     D j               s              3  0 INZ(1)
007800030822     D wGiroDft        s              1  0 INZ(*zeros)
007900080418     D*------------------
008000080418     D* Costanti
008100080418     D*------------------
008200080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400100318
008500100318     D*------------------
008600100318     D* LINKING A DEFINIZIONI ESTERNE
008700100318     D*------------------
008800100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008900100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009000100318
009100010330
009200010330
009300010330
009400000913     C                   reset                   rrnum
009500990921     C                   reset                   esito
009600990921     C                   reset                   wrkesito
009700010601     C*
009800010601     C                   exsr      opeini
009900010605     C                   exsr      rwvab
010000010601     C*
010100010601     C                   seton                                        lr
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010700010601     C*--------------------------------------------------------
010800010601     C     PREELA        BEGSR
010900010601     C*
011000010601     C* SE OCCORRE SPEDIRE IN FILIALE
011100010601     C                   if        invfil <> *zeros and
011200010601     C                             flgGiro = '0'
011300010601     C*
011400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011500010601     C                   eval      flgGiro = '1'
011600010601     C*
011700010601     C                   endif
011800010601     C*
011900010601     C                   ENDSR
012000010601     C***
012100010601
012200010601
012300010601
012400010601
012500010601     C*--------------------------------------------------------
012600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012700010601     C*--------------------------------------------------------
012800010601     C     ENDELA        BEGSR
012900040119     C*
013000040119     C* Effettuo la chiamata al CLLE preposto
013100040119     C                   call(e)   'TITVVTC'
013200040510     C                   parm                    parccm
013300040119     C                   parm                    parmbr
013400040119     C                   parm      '2'           paropz
013500050201     C*
013600050201     C* Effettuo lancio TISI95 solo x chiusura
013700050201     C                   CLEAR                   TISI95DS
013800050201     C                   EVAL      I95TLA = 'C'
013900050201     C                   CALL      'TISI95R'
014000050201     C                   PARM                    TISI95DS
014100000616     C*
014200010601     C                   ENDSR
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
017000120410     C*
017100120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017200120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017300120410     C                   IF        Intestazioni = *off
017400120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017500120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017600120410     C                   exsr      inzvar
017700120410     C                   exsr      impvab
017800120410     C                   ENDIF
017900010330     C*
018000030822     C                   do        *HIVAL
018100010330     C*
018200030822     C                   read      tivin00r                               70
018300010618     C*
018400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018500010618     C                   if        vinflg = '1'
018600010618     C                   eval      flgOk = '1'
018700010618     C                   endif
018800040510     C*
018900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019000040510     C                   z-add     *zeros        wLenVINDTA        4 0
019100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019500040510     C                   z-add     1             wFlgVINDTA
019600040510     C                   leave
019700040510     C                   endif
019800040510     C                   enddo
019900010618     C*
020000040510     C                   if        vindta > *blanks AND
020100040510     C                             wFlgVINDTA = 1
020200010330     C                   add       1             rrnum
020300010330     C*
020400010601     C                   if        *in70 = *off and
020500010330     C                             (vinflg = *blanks
020600010330     C                              or vinflg = '0'
020700010330     C                              or vinflg = '2')
020800010330     C*
020900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021000010711     C                   if        vinflg = *blanks or vinflg = '0'
021100010711     C                   clear                   vinmsg
021200010711     C                   endif
021300130917     C*
021400130917     C* Aggiungo un byte a blanks all'inizio d ogni record
021500130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021600130917     C* evitare che vengano elisi i blank
021700130917     C                   eval      vindta  = '*' + %trim(vindta)
021800130917     C*
021900080527     C                   exsr      inzvar
022000100318     C                   exsr      normCSV
022100010605     C                   exsr      impvab
022200010601     C*
022300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022400010605     C                   eval      depfil = VABLNP
022500010601     C                   exsr      repfil
022600010601     C                   if        depfil = invfil
022700021025     C                   if        vlrpoi = 999
022800030822     C                   move(P)   invfil        VABFGS
022900021025     C                   else
023000030822     C                   move(P)   vlrpoi        VABFGS
023100021025     C                   endif
023200010601     C*
023300010601     C                   exsr      PREELA
023400010601     C*
023500010604     C* Ebbene...
023600010604     C*
023700040119     C  N31              add       1             §CTROKVB
023800040119     C   32              add       1             §CTRMO
023900040119     C   31              add       1             §CTRNO
024000080418     C*
024100030822     C                   if        wGiro = 2
024200080513     C* Scrivo sempre il VAT
024300080513     C  N31              exsr      wrivat
024400110201     C                   exsr      chkvab
024500080513     C                   endif
024600010604     C*
024700010604     C                   if        *in31 = *off and
024800010604     C                             *in32 = *off
024900010604     C                   eval      vinflg = '1'
025000010604     C                   else
025100010604     C                   eval      vinflg = '2'
025200010604     C                   endif
025300010604     C                   endif
025400010601     C*
025500010604     C                   endif
025600010604     C*
025700010330     C                   else
025800010330     C                   eval      vinflg = '1'
025900010330     C                   endif
026000010601     C*
026100010601     C  N70              update    tivin000
026200010330     C*
026300030822     C  N70              enddo
026400040119     C*
026500080527     C* Verifico dati rimasti "in cannna"...
026600080527     C                   seton                                        65
026700110201     C                   exsr      chkvab
026800080527     C*
026900040119     C                   endif                                                  (endif - chkcall)
027000010601     C*
027100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027200010601     C                   if        cntNonEl = *zeros or
027300010601     C                             flgMulti = '0'
027400010330     C* Se non ci sono record con errori ...
027500010601     C                   if        §ctrno = 0 and
027600010604     C                             §ctrmo = 0 and
027700010601     C                             flgStato <> '2'
027800010330     C* ... restituisco esito OK.
027900010330     C                   eval      wrkesito = '0'
028000010330     C                   else
028100040119     C                   if        §ctrokvb > 0
028200010330     C                   eval      wrkesito = '1'
028300010330     C                   else
028400010615     C                   if        flgOk = '0'
028500010615     C                   eval      wrkesito = '2'
028600010615     C                   else
028700010615     C                   eval      wrkesito = '6'
028800010615     C                   endif
028900010330     C                   endif
029000010330     C                   endif
029100010601     C                   else
029200010601     C                   eval      wrkesito = '9'
029300010601     C                   endif
029400010330     C*
029500010330     C                   if        %open(tivin00r)
029600010330     C                   close     tivin00r
029700010330     C                   endif
029800021025     C                   if        %open(fivabwwr)
029900021025     C                   close     fivabwwr
030000010330     C                   endif
030100040512     C                   if        %open(fivatwwr)
030200040512     C                   close     fivatwwr
030300040512     C                   endif
030400010601     C*
030500010601     C                   if        vlrpoi <> 999
030600010601     C                   eval      invfil = vlrpoi
030700010601     C                   endif
030800010330     C*
030900040119     C                   if        §ctrokvb > 0
031000010601     C                             and invfil > *zeros
031100010330     C                   exsr      invio
031200010330     C                   endif
031300010601     C*
031400010618     C                   if        flgGiro = '1'
031500010601     C                   exsr      endela
031600010618     C                   endif
031700010330     C*
031800010330     C                   ENDSR
031900010330     C***
032000080527
032100080527
032200080527
032300080527     C*----------------------------------------------------*
032400080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032500080527     C*----------------------------------------------------*
032600100413     C     CHKVAB        BEGSR
032700110201     C*
032800110201     C* Se richiesti ignorare colli / peso / volume
032900110315 xxx C***                if        *in35
033000110315 xxx C***                z-add     *zeros        VABNCL
033100110315 xxx C***                z-add     *zeros        VABPKB
033200110315 xxx C***                z-add     *zeros        VABVLB
033300110315 xxx C***                endif
033400080527     C*
033500080527     C* Scrivo il VAB solo a rottura d spedizione
033600080527     C                   if        depspe = *zeros
033700080527     C  N31              eval      fivabds_sav = fivabds
033800080527     C                   eval      depspe = vabNSP
033900080527     C                   else
034000080527     C                   if        depspe = vabNSP AND *in65 = *off
034100100413     C   51              add       VABNCL        sav_VABNCL
034200100413     C   51              add       VABPKB        sav_VABPKB
034300100413     C   51              add       VABVLB        sav_VABVLB
034400100720     C   52              z-add     VABNCL        sav_VABNCL
034500100720     C   52              z-add     VABPKB        sav_VABPKB
034600100720     C   52              z-add     VABVLB        sav_VABVLB
034700100720     C   53              add       VABNCL        sav_VABNCL
034800100720     C   53              z-add     VABPKB        sav_VABPKB
034900100720     C   53              z-add     VABVLB        sav_VABVLB
035000080527     C                   else
035100080527     C  N31              eval      fivabds_ok = fivabds_sav
035200080527     C  N31              exsr      wrivab
035300080527     C  N31              eval      fivabds_sav = fivabds
035400080527     C                   eval      depspe = vabNSP
035500080527     C                   endif
035600080527     C                   endif
035700080527     C*
035800080527     C                   ENDSR
035900040119
036000040119
036100040119
036200040119     C*----------------------------------------------------*
036300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036400040119     C*----------------------------------------------------*
036500040510     C     WRIVAB        BEGSR
036600080513     C*
036700080513     C* Gestisco l'eventuale rottura x numero spedizione
036800080527     C                   if        VABNSP <> depspe or *in65 = *on
036900070301     C                   if        wVATNOT_E <> *blanks
037000130610     C                   eval      ok_VABCTM = '7Q'
037100070301     C                   endif
037200100413     C                   write     fivab000
037300080513     C                   endif
037400040119     C*
037500040119     C                   ENDSR
037600040119
037700040119
037800040119
037900040119     C*----------------------------------------------------*
038000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038100040119     C*----------------------------------------------------*
038200040510     C     WRIVAT        BEGSR
038300060512     C*
038400060512     C* Inizializzo i campi chiave
038500060512     C                   eval      VATFGS = VABFGS
038600060512     C                   eval      VATAAS = VABAAS
038700060512     C                   eval      VATCCM = VABCCM
038800070823     C                   eval      VATNRS = VABNRS
038900060512     C                   eval      VATNSP = VABNSP
039000060512     C                   eval      VATLNP = VABLNP
039100040119     C*
039200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039300040510     C                   if        VATTRC = *blanks
039400040510     C                   eval      VATTRC = 'E'
039500040510     C                   endif
039600110318     C                   if        VATNOT <> *blanks
039700040510     C                   write     FIVAT000
039800040119     C                   add       1             §CTROKVT
039900110318     C                   endif
040000060331     C*
040100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040200060331     C                   if        wVATNOT_A <> *blanks
040300060331     C                   eval      VATNOT = wVATNOT_A
040400060331     C                   eval      VATTRC = 'A'
040500060331     C                   write     FIVAT000
040600060331     C                   add       1             §CTROKVT
040700060331     C                   endif
040800060331     C*
040900060331     C                   if        wVATNOT_B <> *blanks
041000060331     C                   eval      VATNOT = wVATNOT_B
041100060331     C                   eval      VATTRC = 'B'
041200060331     C                   write     FIVAT000
041300060331     C                   add       1             §CTROKVT
041400060331     C                   endif
041500060331     C*
041600120320     C*
041700120410     C* i rcd di FIVAT con tipo E sono 1 in quanto è una spedizione monocollo
041800060331     C                   if        wVATNOT_E <> *blanks
041900110201     C                   eval      VATNOT = wVATNOT_E
042000060331     C                   eval      VATTRC = 'E'
042100120411     C*
042200120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
042300120411     C*
042400120411     C     1             DO        VABNCL        wNumCollo         3 0
042500130611     C*** da analisi del POC il progressivo colli è max 3 char e deve contenere gli zeri non
042600120411     C*** significativi
042700120411     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
042800120411     C                                %editc(wNumCollo:'X')
042900130611     C* recupero il check digit
043000130611     C                   clear                   TRUL28DSX
043100130611     C                   eval      I284STD = 'E10'
043200130611     C                   eval      I284COD = VATNOT
043300130611     C                   call      'TRUL28R4'
043400130611     C                   parm                    TRUL28DSX
043500130611     C* Se tutto ok scrivo il "chi sono" corrente col check digit
043600130611     C                   IF        O284err = *blanks
043700130611     C                   EVAL      VATNOT = %trim(VATNOT)
043800130611     C                              + %trim(O284CkD)
043900120410     C                   write     FIVAT000
044000120410     C                   add       1             §CTROKVT
044100130611     C                   ELSE
044200130611     C                   SETON                                        31
044300130611     C                   EVAL      VATNOT = *blank
044400130611     C                   EVAL      vinmsg = %trimr(vinmsg)
044500130611     C                             + ' ' + 'VATNOT'
044600130611     C                   ENDIF
044700120411     C                   ENDDO
044800120320     C*
044900120320     C                   endif
045000120320     C*
045100110927     C*
045200110927     C                   if        wVATNOT_I <> *blanks
045300110927     C                   eval      VATNOT = wVATNOT_I
045400110927     C                   eval      VATTRC = 'I'
045500110927     C                   write     FIVAT000
045600110927     C                   add       1             §CTROKVT
045700110927     C                   endif
045800040119     C*
045900040119     C                   ENDSR
046000040119
046100040119
046200040119
046300040119     C*----------------------------------------------------*
046400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046500040119     C*----------------------------------------------------*
046600040119     C     PREVAT        BEGSR
046700040119     C*
046800040119     C* Compongo il nome del membro da dare al FIVATWWR
046900040119     C                   eval      parmbr = vlrhdl
047000040119     C                   movel     'M'           parmbr
047100040510     C                   eval      parccm = vlrksc
047200040119     C                   eval      paropz = '1'
047300040119     C* Effettuo la chiamata al CLLE preposto
047400040119     C                   call(e)   'TITVVTC'
047500040119     C                   parm                    parccm
047600040119     C                   parm                    parmbr
047700040119     C                   parm                    paropz
047800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047900040119     C                   if        %error
048000040119     C                   movel     '1'           chkcall
048100040119     C                   else
048200040119     C                   movel     '0'           chkcall
048300040119     C                   endif
048400040119     C*
048500040119     C                   ENDSR
048600010601
048700010601
048800010601
048900010330     C*----------------------------------------------------*
049000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049100010330     C*----------------------------------------------------*
049200040119     C     INZVAR        BEGSR
049300080527     C*
049400080527     C                   setoff                                       65
049500010330     C*
049600030822     C* Inizializzo variabili di wrk
049700020204     C                   Z-ADD     *zeros        Num5_0            5 0
049800101018     C                   MOVEL     *blanks       wFlgCAS
049900060331     C                   MOVEL     *blanks       wVATNOT_A
050000060331     C                   MOVEL     *blanks       wVATNOT_B
050100060331     C                   MOVEL     *blanks       wVATNOT_E
050200110927     C                   MOVEL     *blanks       wVATNOT_I
050300120411     C                   MOVEL     *blanks       wNOT
050400120411     C                   MOVEL     *blanks       Data8A
050500030822     C*
050600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050700030822     C                   CLEAR                   SkSplitCSV
050800030822     C                   EVAL      i = 1
050900030822     C                   EVAL      posDa = *zeros
051000030822     C                   EVAL      posA  = *zeros
051100080527     C*
051200080527     C                   CLEAR                   FIVABDS
051300080527     C                   CLEAR                   FIVAT000
051400030822     C*
051500030822     C* Reimposto i valori di default
051600030822     C                   EXSR      DEFCAM
051700030822     C*
051800010330     C                   ENDSR
051900010330     C*----------------------------------------------------*
052000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052100010330     C*----------------------------------------------------*
052200010330     C     DEFCAM        BEGSR
052300030822     C*
052400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052500030715     C* e delimitatore testo.
052600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052900100318     C*
053000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053100100318     C                   EVAL      CharSOS = CharNUM
053200030822     C*
053300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053400100720     C                   SETOFF                                       505152
053500100720     C                   SETOFF                                       53
053600100413     C                   SELECT
053700100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053800100720     C                   SETON                                        50
053900100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
054000100413     C                   SETON                                        51
054100100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
054200100720     C                   SETON                                        52
054300100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
054400100720     C                   SETON                                        53
054500100413     C                   ENDSL
054600030822     C*
054700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054800030822     C                   EVAL      posDaDft = 1
054900030822     C                   EVAL      posADft  = 0
055000030822     C                   EVAL      wGiroDft = 0
055100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055200030822     C                             posDaDft > 0
055300030822     C*
055400030822     C* Gestisco il 1° giro
055500030822     C                   IF        wGiroDft = 0
055600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055800030822     C* Incremento il contatore dei "giri"
055900030822     C                   EVAL      wGiroDft = 1
056000030822     C                   ELSE
056100030822     C                   EVAL      posDaDft = posADft
056200030822     C                   ENDIF
056300030822     C* Eseguo lo scan x trovare la fine del campo corrente
056400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056500030822     C*
056600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056800030822     C                   IF        posDaDft > 0 AND
056900030822     C                             posADft  > 0
057000030822     C* NCL
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'NCL'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiInt=*on
058000030822     C                   Z-ADD     PiVal         VABNCL
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* CCM
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'CCM'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiInt=*on
059300030822     C                   Z-ADD     PiVal         VABCCM
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* LNP
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'LNP'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600030822     C                   Z-ADD     PiVal         VABLNP
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* NRS
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'NRS'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiInt=*on
061900030822     C                   Z-ADD     PiVal         VABNRS
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* CTR
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'CTR'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiInt=*on
063200030822     C                   Z-ADD     PiVal         VABCTR
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* PKB
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'PKB'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiNum=*on
064500130610     C                   Z-ADD     PiVal         VABPKB
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* VLB
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'VLB'
065300030822     C                   EVAL      PiStr=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   EXSR      CHKNUM
065700030822     C                   IF        PiNum=*on
065800130610     C                   Z-ADD     PiVal         VABVLB
065900030822     C                   ENDIF
066000030822     C                   ENDIF
066100030822     C* QFT
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'QFT'
066600030822     C                   EVAL      PiStr=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   EXSR      CHKNUM
067000030822     C                   IF        PiNum=*on
067100030822     C                   Z-ADD     PiVal         VABQFT
067200030822     C                   ENDIF
067300030822     C                   ENDIF
067400030822     C* CBO
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'CBO'
067900030822     C                   EVAL      VABCBO=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300030822     C* TSP
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'TSP'
068800030822     C                   EVAL      VABTSP=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200030822     C* VAS
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'VAS'
069700030822     C                   EVAL      VABVAS=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100030822     C* VCA
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'VCA'
070600030822     C                   EVAL      VABVCA=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000070925     C* TIC
071100070925     C                   IF        %subst(
071200070925     C                             %subst(vlrppt:posDaDft+1:
071300070925     C                             posADft-posDaDft-1):1:3)
071400070925     C                             = 'TIC'
071500070925     C                   EVAL      VABTIC=%trim(%subst(
071600070925     C                             %subst(vlrppt:posDaDft+1:
071700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800070925     C                   ENDIF
071900030822     C* GCA
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'GCA'
072400030822     C                   EVAL      VABGCA=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* CTM
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'CTM'
073300030822     C                   EVAL      VABCTM=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* FFD
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'FFD'
074200030822     C                   EVAL      VABFFD=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* VAD
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'VAD'
075100030822     C                   EVAL      VABVAD=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* GMA
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'GMA'
076000030822     C                   EVAL      VABGMA=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* GGA
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'GGA'
076900030822     C                   EVAL      VABGGA=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300030822     C* GVA
077400030822     C                   IF        %subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):1:3)
077700030822     C                             = 'GVA'
077800030822     C                   EVAL      VABGVA=%trim(%subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100030822     C                   ENDIF
078200030822     C* TC1
078300030822     C                   IF        %subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):1:3)
078600030822     C                             = 'TC1'
078700030822     C                   EVAL      VABTC1=%trim(%subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000030822     C                   ENDIF
079100030822     C* TC2
079200030822     C                   IF        %subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):1:3)
079500030822     C                             = 'TC2'
079600030822     C                   EVAL      VABTC2=%trim(%subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079900030822     C                   ENDIF
080000040714     C* VATTRC
080100040714     C                   IF        %subst(
080200040714     C                             %subst(vlrppt:posDaDft+1:
080300040714     C                             posADft-posDaDft-1):1:3)
080400040714     C                             = 'TRC'
080500040714     C                   EVAL      VATTRC=%trim(%subst(
080600040714     C                             %subst(vlrppt:posDaDft+1:
080700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080800040714     C                   ENDIF
080900030822     C* ...
081000030822     C                   ENDIF
081100030822     C                   ENDDO
081200020204     C*
081300010330     C                   ENDSR
081400010607     C*----------------------------------------------------*
081500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081600010607     C*----------------------------------------------------*
081700010607     C     IMPVAB        BEGSR
081800010607     C*
081900010607     C                   SETOFF                                       3132
082000110315 xxx C***                SETON                                        35
082100030515     C*
082200010607     C                   MOVEL     datcor        VABAAS
082300010607     C                   MOVE      datcor        VABMGS
082400010607     C*
082500030715     C**********
082600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082700030715     C**********
082800030715     C                   IF        wGiro = *zeros
082900040113     C*
083000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083300080429     C                             %trim(intcol(5))<>*blanks
083400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083600080429     C                                      %trim(intcol(5))
083700070823     C     ';':CharCSV   XLATE     vindta        vindta
083800040510     C                   ENDIF
083900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084000031201     C     minu:maiu     XLATE     vindta        vindta
084100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084200030715     C                   DOW       posDa <= %len(%trim(vindta))
084300030715     C*
084400030715     C* Gestisco il 1° campo
084500030715     C                   IF        i = 1
084600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084700030715     C                   EVAL      posDa = 1
084800030715     C* Eseguo lo scan x trovare la fine del primo campo
084900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085000030715     C* A questo punto "estraggo" il campo corrente
085100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085200030715     C                                                    (posA-posDa))
085300030715     C* X i campi successivi al 1°
085400030715     C                   ELSE
085500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085600030715     C                   EVAL      posDa = posA + 1
085700030715     C* Eseguo lo scan x trovare la fine del campo corrente
085800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085900030715     C* Gestisco l'ultimo campo
086000030715     C                   IF        posA = *zeros
086100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C                   LEAVE
086600030715     C                   ELSE
086700030715     C* A questo punto "estraggo" il campo corrente
086800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086900030715     C                                                    (posA-posDa))
087000030715     C                   ENDIF
087100030715     C                   ENDIF
087200030715     C* Incremento il contatore d campo
087300030715     C                   EVAL      i = i +1
087400030715     C                   ENDDO
087500030715     C*
087600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087700030715     C* delimitazione testo
087800030715     C                   EVAL      i = 1
087900030715     C                   DOW       i < %elem(SkSplitFLD)
088000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088200030715     C                   EVAL      i = i + 1
088300030715     C                   ENDDO
088400030715     C                   EVAL      wGiro = 1
088500030715     C                   ELSE
088600030715     C                   EVAL      wGiro = 2
088700031204     C**********
088800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088900031204     C* d separatore campo
089000031204     C**********
089100031204     C                   EVAL      vindta = %trim(vindta)
089200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089300031204     C                   EVAL      vindta = %subst(vindta:2)
089400031204     C                   ENDDO
089500031204     C*
089600031204     C                   Z-ADD     *zeros        lunghInput        4 0
089700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090100110912     C                   ENDDO
090200030715     C**********
090300030715     C* Effettuo lo split del campo dati d input
090400030715     C**********
090500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090600030715     C                   DOW       posDa <= %len(%trim(vindta))
090700030715     C*
090800030715     C* Gestisco il 1° campo
090900030715     C                   IF        i = 1
091000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091100030715     C                   EVAL      posDa = 1
091200030715     C* Eseguo lo scan x trovare la fine del primo campo
091300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C* X i campi successivi al 1°
091800030715     C                   ELSE
091900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092000030715     C                   EVAL      posDa = posA + 1
092100031204     C* Verifico che nn vi sia il campo nn valorizzato
092200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092300031204     C* Se campo nn valorizzato skippo al prossimo
092400031204     C                   EVAL      SkSplitCSV(i) = *blanks
092500031204     C                   EVAL      posA  = posA + 1
092600031204     C                   ELSE
092700030715     C* Eseguo lo scan x trovare la fine del campo corrente
092800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092900030715     C* Gestisco l'ultimo campo
093000030715     C                   IF        posA = *zeros
093100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093200030715     C* A questo punto "estraggo" il campo corrente
093300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093400030715     C                                                    (posA-posDa))
093500030715     C                   LEAVE
093600030715     C                   ELSE
093700030715     C* A questo punto "estraggo" il campo corrente
093800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093900030715     C                                                    (posA-posDa))
094000030715     C                   ENDIF
094100031204     C                   ENDIF
094200030715     C                   ENDIF
094300030715     C* Incremento il contatore d campo
094400030715     C                   EVAL      i = i +1
094500030715     C                   ENDDO
094600030715     C*
094700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094800030715     C* delimitazione testo
094900030715     C                   EVAL      i = 1
095000030715     C                   DOW       i < %elem(SkSplitCSV)
095100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095300030715     C                   EVAL      i = i + 1
095400030715     C                   ENDDO
095500100720     C*
095600100720     C* Stacco un progressivo x numero spedizione
095700130917 xxx C                   EXSR      repNSP
095800100326     C*
095900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096000030715     C                   EVAL      i = 1
096100030715     C                   DOW       i < %elem(SkSplitFLD)
096200030715     C*
096300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096500030715     C                             SkSplitCSV(i) <> *blanks
096600030721     C*** ATB
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096800030721     C                   EVAL      VABATB = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030715     C*** RSD
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097500030715     C* ==
097600130528     C                   ENDIF
097700030721     C*** RD2
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030715     C*** IND
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098300030715     C                   EVAL      VABIND = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030715     C*** LOD
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** PRD
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030721     C*** NZD
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030715     C*** RMA
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030715     C*** NOT
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030715     C*** NT2
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** VCA
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** VAS
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** FFD
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030721     C*** GC1
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** GC2
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TSP
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103200030721     C                   ENDIF
103300040510     C*** CBO
103400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103600040510     C                   ENDIF
103700030721     C*** NAS
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** TIC
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** GCA
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** XCO
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** CTM
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** TCR
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** CTS
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** FTM
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** VAD
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** GMA
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** GGA
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** GVA
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TC1
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** TC2
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** SCL
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** RMO
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** NMO
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*
110600030721     C* Reperisco quindi i campi numerici...
110700040510     C*** NSP
110800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110900040510     C                   EVAL      PiStr=SkSplitCSV(i)
111000040510     C                   EXSR      CHKNUM
111100040510     C                   IF        PiInt=*on
111200040510     C                   Z-ADD     PiVal         VABNSP
111300040510     C                   ELSE
111400040510     C                   SETON                                        31
111500040510     C                   EVAL      VABNSP = *zeros
111600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111700040510     C                             + ' ' + 'VABNSP'
111800040510     C                   ENDIF
111900040510     C                   ENDIF
112000071106     C*** RMN
112100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112200071106     C                   EVAL      PiStr=SkSplitCSV(i)
112300071106     C                   EXSR      CHKNUM
112400071106     C                   IF        PiInt=*on
112500071106     C                   Z-ADD     PiVal         VABRMN
112600071106     C                   ELSE
112700071106     C                   SETON                                        32
112800071106     C                   EVAL      VABRMN = *zeros
112900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113000071106     C                             + ' ' + 'VABRMN'
113100071106     C                   ENDIF
113200071106     C                   ENDIF
113300030721     C*** CAD
113400130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113500130610     C                   EVAL      PiStr=SkSplitCSV(i)
113600130610     C                   EXSR      CHKNUM
113700130610     C                   IF        PiInt=*on
113800130610     C                   Z-ADD     PiVal         Num5_0
113900130610     C                   MOVEL(p)  Num5_0        VABCAD
114000130610     C                   ELSE
114100130610     C                   SETON                                        32
114200130610     C                   EVAL      VABCAD = *zeros
114300130610     C                   EVAL      vinmsg = %trimr(vinmsg)
114400130610     C                             + ' ' + 'VABCAD'
114500130610     C                   ENDIF
114600130605     C                   ENDIF
114700030721     C*** NCL
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on
115200030721     C                   Z-ADD     PiVal         VABNCL
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABNCL = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700040122     C                             + ' ' + 'VABNCL'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** PKB
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiNum=*on
116500130610     C                   Z-ADD(H)  PiVal         VABPKB
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABPKB = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABPKB'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** CAS
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117500110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
117600111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
117700111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
117800130703     C                   EVAL      VABVCA = 'EUR'
117900101018     C                   ENDIF
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiNum=*on
118300030721     C                   Z-ADD     PiVal         VABCAS
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABCAS = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABCAS'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** CCM
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on      AND
119600030721     C                             PiVal<=9999999 AND
119700030721     C                             PiVal>*zeros
119800030721     C                   Z-ADD     PiVal         VABCCM
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      VABCCM = *zeros
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300040510     C                             + ' ' + 'VABCCM'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** LNP
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on    AND
121100030721     C                             PiVal<=999   AND
121200030721     C                             PiVal>*zeros
121300030721     C                   Z-ADD     PiVal         VABLNP
121400030721     C                   ELSE
121500040715     C                   SETON                                        32
121600030721     C                   EVAL      VABLNP = *zeros
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800040510     C                             + ' ' + 'VABLNP'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** NRS
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiInt=*on AND
122600030721     C                             PiVal<=99
122700030721     C                   Z-ADD     PiVal         VABNRS
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABNRS'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** LNA
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on    AND
123900030721     C                             PiVal<=999   AND
124000030721     C                             PiVal>*zeros
124100030721     C                   Z-ADD     PiVal         VABLNA
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABLNA = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABLNA'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** CTR
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on  AND
125400030721     C                             PiVal<=999
125500030721     C                   Z-ADD     PiVal         VABCTR
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABCTR'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** IAS
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiNum=*on
126700030721     C                   Z-ADD     PiVal         VABIAS
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABIAS'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** VLB
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiNum=*on
127900130610     C                   Z-ADD(H)  PiVal         VABVLB
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABVLB'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** QFT
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiNum=*on
129100030721     C                   Z-ADD     PiVal         VABQFT
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABQFT'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** NCD
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiInt=*on      AND
130300030721     C                             PiVal<=9999999
130400030721     C                   Z-ADD     PiVal         VABNCD
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABNCD'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** NCA
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiInt=*on      AND
131600030721     C                             PiVal<=9999999
131700030721     C                   Z-ADD     PiVal         VABNCA
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABNCA'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** ZNC
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on AND
132900030721     C                             PiVal<=99
133000030721     C                   Z-ADD     PiVal         VABZNC
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABZNC'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030721     C*** DCR
133800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on       AND
134200030721     C                             PiVal<=99999999
134300030721     C                   Z-ADD     PiVal         VABDCR
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134700030721     C                             + ' ' + 'VABDCR'
134800030721     C                   ENDIF
134900030721     C                   ENDIF
135000030721     C*** HCR
135100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135200030721     C                   EVAL      PiStr=SkSplitCSV(i)
135300030721     C                   EXSR      CHKNUM
135400030721     C                   IF        PiInt=*on   AND
135500030721     C                             PiVal<=9999
135600030721     C                   Z-ADD     PiVal         VABHCR
135700030721     C                   ELSE
135800030721     C                   SETON                                        32
135900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136000030721     C                             + ' ' + 'VABHCR'
136100030721     C                   ENDIF
136200030721     C                   ENDIF
136300030721     C*** VMD
136400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136500030721     C                   EVAL      PiStr=SkSplitCSV(i)
136600030721     C                   EXSR      CHKNUM
136700030721     C                   IF        PiNum=*on
136800030721     C                   Z-ADD     PiVal         VABVMD
136900030721     C                   ELSE
137000030721     C                   SETON                                        32
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABVMD'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500030721     C*** ANT
137600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiInt=*on        AND
138000030721     C                             PiVal<=999999999
138100030721     C                   Z-ADD     PiVal         VABANT
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABANT'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800030721     C*** CMO
138900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139000030721     C                   EVAL      PiStr=SkSplitCSV(i)
139100030721     C                   EXSR      CHKNUM
139200030721     C                   IF        PiInt=*on
139300030721     C                   Z-ADD     PiVal         Num5_0
139400030721     C                   MOVEL(p)  Num5_0        VABCMO
139500030721     C                   ELSE
139600030721     C                   SETON                                        32
139700030721     C                   EVAL      VABCMO = *zeros
139800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139900030721     C                             + ' ' + 'VABCMO'
140000030721     C                   ENDIF
140100030721     C                   ENDIF
140200040510     C*
140300040510     C* Reperisco i campi del FIVAT
140400040510     C*** VATNOT
140500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140700040510     C                   ENDIF
140800040510     C*** VATTRC
140900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141100040510     C                   ENDIF
141200060331     C*** VATNOT_A
141300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141500060331     C                   ENDIF
141600060331     C*** VATNOT_B
141700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141900060331     C                   ENDIF
142000060331     C*** VATNOT_E
142100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142300060331     C                   ENDIF
142400110927     C*** VATNOT_I
142500110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
142600110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
142700110927     C                   ENDIF
142800070823     C*
142900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
143000120301
143100130917      *** W_VABRSD: ciò che eccede i 35 char va in VABRD2
143200130917     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
143300130917     C                   MOVEL     *blanks       wRSD             70
143400130917     C                   EVAL      wRSD=%trim(SkSplitCSV(i))
143500130917     C                   EVAL      VABRSD=%subst(wRSD:1:35)
143600130917     C                   EVAL      VABRD2=%subst(wRSD:36:35)
143700130917     C                   ENDIF
143800130917
143900130917      *** W_VABCBO: se valore KB = 4, se valore 0=1
144000130917     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
144100130917     C                   SELECT
144200130917     C                   WHEN      %trim(SkSplitCSV(i)) = 'KB'
144300130917     C                   EVAL      VABCBO='4'
144400130917     C                   WHEN      %trim(SkSplitCSV(i)) = '0'
144500130917     C                   EVAL      VABCBO='1'
144600130917     C                   ENDSL
144700130917     C                   ENDIF
144800130626
144900070925     C*
145000071029     C***  <===  -----------------------------------------
145100040510     C*
145200030715     C                   ENDIF
145300030715     C                   EVAL      i = i + 1
145400030715     C                   ENDDO
145500070925     C*
145600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145700120411     C*
145800130917     C* VABRMN= VABNSP
145900130917     C                   EVAL      VABRMN = VABNSP
146000130917     C*
146100130917     C* VABRMA= rappresentazione a stringa editata del valore del campo VABPKB
146200130917     C                   EVAL      VABRMA = %trim(%editc(VABPKB:'1'))+' KG'
146300070925     C*
146400071029     C***  <===  -----------------------------------------
146500070925     C*
146600070925     C* Considerazioni finali su CBO/CAS
146700130917     C* siccome è il file del cliente che pilota CBO, pulisco solo il contrassegno se porto franco
146800070925     C                   IF        VABCBO = '1'
146900130917     C                   EVAL      VABCAS = 0
147000070925     C                   ENDIF
147100070925     C***  <===  ----------------------------
147200040510     C*
147300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147400040510     C                   IF        VABNSP = *zeros
147500040510     C                   SETON                                        31
147600040510     C                   ENDIF
147700040715     C*
147800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147900040715     C                   IF        VABLNP = *zeros
148000040715     C                   SETON                                        31
148100040715     C                   ENDIF
148200040510     C*
148300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148400120411     C                   IF        VABRMN = *zeros
148500120411     C                   Z-ADD     VABNSP        VABRMN
148600120411     C                   ENDIF
148700120328     C* IN QUESTO CASO LO FORZO = 1
148800120411     C***                IF        VABRMN = *zeros
148900120411     C***                Z-ADD     1             VABRMN
149000120411     C***                ENDIF
149100040510     C*
149200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149300040510     C                   IF        VABRMA = *blanks
149400040714     C                   MOVEL     VABRMN        VABRMA
149500040510     C                   ENDIF
149600040128     C*
149700040510     C* Se provincia nn valorizzata la reperisco
149800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149900110912     C                   IF        VABPRD =  *blanks AND
150000110912     C                             VABNZD =  *blanks AND
150100110912     C                             VABCAD <> *blanks AND
150200110912     C                             VABLOD <> *blanks
150300040128     C                   CLEAR                   TISI95DS
150400040128     C                   EVAL      I95TCN = '3'
150500040128     C                   Z-ADD     datcor        I95DAT
150600040128     C                   EVAL      I95NAR = VABNZD
150700040128     C                   EVAL      I95CAP = VABCAD
150800040128     C                   EVAL      I95LOC = VABLOD
150900040128     C                   CALL      'TISI95R'
151000040128     C                   PARM                    TISI95DS
151100040128     C                   EVAL      VABPRD = O95PRV
151200040510     C                   ENDIF
151300020204     C*
151400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151500020204     C                   EXSR      CHKIMPDIV
151600030715     C                   ENDIF
151700020204     C*
151800010607     C                   ENDSR
151900010607     C*----------------------------------------------------*
152000010601
152100020204
152200020204     C*----------------------------------------------------*
152300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152400020204     C*----------------------------------------------------*
152500020204     C     CHKIMPDIV     BEGSR
152600020204     C*
152700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152800020204     C                   Z-ADD     *zeros        wrkDec            9 9
152900020204     C*
153000020204     C* Come prima cosa effettuo considerazioni sulla divisa
153100020204     C                   IF        vabIAS > *zeros
153200020204     C                   IF        vabVAS <> 'EUR'
153300020204     C                   EVAL      vabVAS =  'ITL'
153400020204     C                   ENDIF
153500020204     C                   ENDIF
153600020204     C*
153700020204     C                   IF        vabCAS > *zeros
153800020204     C                   IF        vabVCA <> 'EUR'
153900020204     C                   EVAL      vabVCA =  'ITL'
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C                   IF        vabVMD > *zeros
154400020204     C                   IF        vabVAD <> 'EUR'
154500020204     C                   EVAL      vabVAD =  'ITL'
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155000020204     C                   Z-ADD     vabIAS        wrkDec
155100020204     C                   IF        wrkDec > *zeros
155200020204     C                   IF        vabVAS = 'ITL'
155300020204     C                   EVAL      vabIAS = *zeros
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155800020204     C                   Z-ADD     vabCAS        wrkDec
155900020204     C                   IF        wrkDec > *zeros
156000020204     C                   IF        vabVCA = 'ITL'
156100020204     C                   EVAL      vabCAS = *zeros
156200020204     C                   ENDIF
156300020204     C                   ENDIF
156400020204     C*
156500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156600020204     C                   Z-ADD     vabVMD        wrkDec
156700020204     C                   IF        wrkDec > *zeros
156800020204     C                   IF        vabVAD = 'ITL'
156900020204     C                   EVAL      vabVMD = *zeros
157000020204     C                   ENDIF
157100020204     C                   ENDIF
157200020204     C*
157300020204     C                   ENDSR
157400020204     C***
157500010330
157600100318
157700100318
157800100318     C*----------------------------------------------------*
157900100318     C*  CONTROLLO NUMERICITA' CAMPI
158000100318     C*----------------------------------------------------*
158100100318     C     CHKNUM        BEGSR
158200100318     C*
158300100318     C                   IF        PiDecChr = *blanks
158400100318     C                   EVAL      PiDecChr = CharNUM
158500100318     C                   ENDIF
158600100318     C*
158700100318     C                   callp     UBISNUM_Check(PiStr
158800100318     C                                          :PiDecChr
158900100318     C                                          :PiVal
159000100318     C                                          :PiNum
159100100318     C                                          :PiInt)
159200100318     C*
159300100318     C                   ENDSR
159400100318     C***
159500010601
159600010601
159700010601
159800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159900010601     C     repfil        BEGSR
160000010601     C*
160100010601     C                   if        invfil = *zeros and
160200010601     C                             depfil > *zeros and
160300010629     C                             (vinflg = *blanks or
160400010629     C                              vinflg = *zeros)
160500010601     C
160600010601     C                   eval      invfil = depfil
160700010601     C                   endif
160800010601     C*
160900010601     C                   if        depfil <> invfil and
161000010601     C                             invfil > *zeros
161100010601     C                   eval      flgMulti = '1'
161200010601     C                   if        vinflg = *blanks
161300010601     C                   add       1             cntNonEl
161400010601     C                   endif
161500010601     C                   endif
161600010601     C*
161700010601     C                   if        vinflg = '2'
161800010601     C                   eval      flgStato = '2'
161900010601     C                   endif
162000010601     C*
162100010601     C                   ENDSR
162200010601     C***
162300010601
162400010601
162500010601
162600010330
162700040119      /TITLE Invio dei dati al punto operativo.
162800040119     C     invio         BEGSR
162900040119     C*
163000040119     C* 1° invio FIVAT
163100040119     C                   reset                   dscmz
163200040119     C                   move      vlrpoi        cmzdst
163300040119     C                   eval      cmzfld = 'FIVATWWR'
163400040119     C                   eval      cmzmbd = vlrhdl
163500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163600040119     C***                if        prmfir = *blanks
163700040119     C                   eval      cmzfla = 'FIVAT00F'
163800040119     C                   eval      cmzmba = 'FIVAT00F'
163900040119     C***                else
164000040119     C***                eval      cmzfla = prmfir
164100040119     C***                eval      cmzmba = prmfir
164200040119     C***                endif
164300040119     C                   eval      cmznrr = *zeros
164400040119     C                   move      §ctrokvt      cmznrr
164500040119     C                   eval      cmzlba = vlrfl1
164600040119     C                   call(e)   'TIS711C'
164700040119     C                   parm                    dscmz
164800040119     C                   parm      *blanks       esito
164900040119     C                   if        %error
165000040119     C                             or cmzerr = '1'
165100040119     C                             or esito  = '1'
165200040119     C                   eval      wrkesito = '3'
165300040119     C                   else
165400040119     C*
165500040119     C* 2° invio FIVAB
165600040119     C                   reset                   dscmz
165700040119     C                   move      vlrpoi        cmzdst
165800040119     C                   eval      cmzfld = vlrfou
165900040119     C                   eval      cmzmbd = vlrhdl
166000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166100040119     C***                if        prmfir = *blanks
166200040119     C                   eval      cmzfla = 'FIVAB00F'
166300040119     C                   eval      cmzmba = 'FIVAB00F'
166400040119     C***                else
166500040119     C***                eval      cmzfla = prmfir
166600040119     C***                eval      cmzmba = prmfir
166700040119     C***                endif
166800040119     C                   eval      cmznrr = *zeros
166900040119     C                   move      §ctrokvb      cmznrr
167000040119     C                   eval      cmzlba = vlrfl1
167100040119     C                   call(e)   'TIS711C'
167200040119     C                   parm                    dscmz
167300040119     C                   parm      *blanks       esito
167400040119     C                   if        %error
167500040119     C                             or cmzerr = '1'
167600040119     C                             or esito  = '1'
167700040119     C                   eval      wrkesito = '3'
167800040119     C                   endif
167900040119     C                   endif
168000040119     C*
168100040119     C                   ENDSR
168200040119     C***
168300010601
168400010601
168500010601
168600010601
168700010601      /TITLE Invio dei dati al punto operativo.
168800010601     C     opeini        BEGSR
168900010601     C*
169000010601     C* Inizializzo flag e contatori operativi
169100010601     C                   movel     '0'           flgGiro           1
169200010601     C                   movel     '0'           flgMulti          1
169300010601     C                   movel     '1'           flgStato          1
169400010615     C                   movel     '0'           flgOk             1
169500010601     C                   z-add     *zeros        cntNonEl         10 0
169600010601     C                   z-add     *zeros        depfil            3 0
169700010601     C                   z-add     *zeros        invfil            3 0
169800010601     C*
169900010601     C                   ENDSR
170000010601     C***
170100070326
170200070326
170300070326
170400070326
170500070326     C     *pssr         BEGSR
170600070329     C*
170700070329     C                   if        %open(tivin00r)
170800070329     C                   close     tivin00r
170900070329     C                   endif
171000070329     C                   if        %open(fivabwwr)
171100070329     C                   close     fivabwwr
171200070329     C                   endif
171300070329     C                   if        %open(fivatwwr)
171400070329     C                   close     fivatwwr
171500070329     C                   endif
171600070326     C*
171700070326     C* Effettuo la chiamata al CLLE preposto
171800070326     C                   call(e)   'TITVVTC'
171900070326     C                   parm                    parccm
172000070326     C                   parm                    parmbr
172100070326     C                   parm      '2'           paropz
172200070326     C*
172300070326     C                   eval      wrkesito = '2'
172400070404     C*
172500070404     C                   seton                                        LR
172600070326     C*
172700070326     C                   ENDSR     '*CANCL'
172800070326     C***
172900100318
173000100318
173100100318
173200100318     C     normCSV       BEGSR
173300100318     C*
173400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173500100318     C* separatori campi
173600100318     C                   if        CharTXT <> *blanks
173700100318     C                   z-add     *zeros        wLenVINDTA        4 0
173800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173900100318     C                   setoff                                       80
174000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174300100318     C                   if        not *in80
174400100318     C                   seton                                        80
174500100318     C                   else
174600100318     C                   setoff                                       80
174700100318     C                   endif
174800100318     C                   endif
174900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
175000100318     C                             and *in80 = *on
175100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175200100318     C                   endif
175300100318     C                   enddo
175400100318     C                   endif
175500100318     C*
175600100318     C                   ENDSR
175700100318     C***
175800070326
175900100326
176000100326
176100100326     C*----------------------------------------------------*
176200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176300100326     C*----------------------------------------------------*
176400100326     C     REPNSP        BEGSR
176500100326     C*
176600100326     C* NSP => Stacco un numeratore da AZNUM
176700100326     C                   clear                   TRUL33DS
176800100326     C                   eval      I33OPE = *zeros
176900100326     C                   eval      I33CNU = 302
177000100326     C                   eval      I33NUM = 1
177100100326     C                   movel     TRUL33DS      KPJBU
177200100326     C                   call      'TRUL33R'
177300100326     C                   parm                    KPJBA
177400100326     C                   movel     KPJBU         TRUL33DS
177500100326     C                   if        O33ERR = *zeros
177600100326     C                   z-add     O33NRF        VABNSP
177700100326     C                   z-add     O33NRF        VATNSP
177800100326     C                   else
177900100326     C                   SETON                                        31
178000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178100100326     C                             + ' ' + 'VABNSP VATNSP'
178200100326     C                   endif
178300100326     C*
178400100326     C                   ENDSR
178500010330
178600100326
178700010330
178800000613     C     *inzsr        BEGSR
178900990910     C*
179000990910     C     *entry        plist
179100990920     C                   parm                    tivlrds
179200990921     C                   parm      wrkesito      esito
179300000724     C                   parm                    prmlit
179400000710     C                   parm                    prmfir
179500010330     C*
179600010330     C* CALCOLA LA DATA CORRENTE
179700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179800100318     C                   eval      datcor = %dec(%date() : *ISO)
179900000613     C*
180000000613     C                   ENDSR
180100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180200130917X;X;X;X;X;X;X;X;X;X;X;W_VABRSD;VABIND;X;X;VABCAD;VABLOD;VATNOT_B;X;X;VABNCL;VABPKB;
180300130917VATNOT_I;X;X;X;X;X;W_VABCBO;X;X;X;X;X;VABCAS;VABVCA;X;
180400130528
180500111116
180600100318
