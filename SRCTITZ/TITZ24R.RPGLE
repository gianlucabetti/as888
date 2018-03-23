000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130618      *
000300130618      * PARTICOLARITA':
000400130619      * - quando c'è c/assegno e il CCM è 56080 occorre valorizzare:
000500130618      * o VABTIC = 'TO'
000600130618      * o VABRMO = 'ARTHEMIA SRL'
000700140116      * gestire la colonna "DECONSE" (Q) secondo quanto di seguito:
000800140116      * - i seguenti valori (attenzione: più valori possibili nel valore del campo) possono essere i
000900140116      * ndicati:
001000140116      * o se presente A          => VABTC1 = 'A'
001100140116      * o se presente P          => VABTC2 = 'P'
001200140116      * o se presente E          => VABTSP = 'E'
001300140116      * o se presente H          => VABTSP = 'H'
001400130618      *
001500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001600120209     F*
001700990910     Ftivin00r  uF   E             DISK    usropn
001800100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001900040119     FFIVATwwr  O    E             DISK    usropn
002000000313     D*
002100040113     D*------------
002200040113     D* SCHIEDE A PROGRAMMA
002300040113     D*------------
002400080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002500010330     D*----------------------------------------------------
002600010330     D* DICHIARAZIOINE VARIABILI DI WRK
002700010330     D*----------------------------------------------------
002800010330     D dscmz         e ds                  inz
002900010330     D psds           sds
003000010330     D  procname         *PROC
003100010330     D tivlrds       e ds                  extname(tivlr00f)
003200040128     D tisi95ds      e ds
003300100413     D fivabds       e ds                  extname(fivab00f)
003400100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003500100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003600010330     D esito           s              1
003700010330     D prmlit          s             10
003800010330     D prmfir          s             10
003900010330     D wrkesito        s                   like(esito)
004000010330     D rrnum           s              6  0 INZ(*zeros)
004100040510     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004600080418     D wFlgCAS         s              1    INZ(*blanks)
004700120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004800120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004900120411     D Intestazioni    s               n   inz(*on)
005000100326     D*------------------
005100100326     D* DS REPERIMENTO NUMERATORE
005200100326     D*------------------
005300100326     D trul33ds      e ds                  inz
005400100326     D*------------------
005500100326     D* DS ARCHITETTURA
005600100326     D*------------------
005700100326     D kpjba         e ds                  inz
005800100326
005900030715     D*------------------
006000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006100030715     D*------------------
006200030715     D SkSplitFLD      S             10    DIM(100)
006300030715     D SkSplitCSV      S            256    DIM(100)
006400030715     D CharCSV         S              1
006500030715     D CharTXT         S              1
006600030715     D CharNUM         S              1
006700100318     D CharSOS         S              1
006800030715     D posDa           S              3  0 INZ(*zeros)
006900030715     D posA            S              3  0 INZ(*zeros)
007000030715     D i               s              3  0 INZ(1)
007100030715     D wGiro           s              1  0 INZ(*zeros)
007200070823     D wVATNOT_A       s                   LIKE(VATNOT)
007300060331     D wVATNOT_B       s                   LIKE(VATNOT)
007400060331     D wVATNOT_E       s                   LIKE(VATNOT)
007500110927     D wVATNOT_I       s                   LIKE(VATNOT)
007600140226     D wVATNOT_J       s                   LIKE(VATNOT)
007700140226     D wVATNOT_IJ      s             70
007800120411     D wNOT            s             70
007900120411     D Data8A          s              8
008000030822     D*------------------
008100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008200030822     D*------------------
008300030822     D posDaDft        S              3  0 INZ(*zeros)
008400030822     D posADft         S              3  0 INZ(*zeros)
008500030822     D j               s              3  0 INZ(1)
008600030822     D wGiroDft        s              1  0 INZ(*zeros)
008700080418     D*------------------
008800080418     D* Costanti
008900080418     D*------------------
009000080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200100318
009300100318     D*------------------
009400100318     D* LINKING A DEFINIZIONI ESTERNE
009500100318     D*------------------
009600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009800100318
009900010330
010000010330
010100010330
010200000913     C                   reset                   rrnum
010300990921     C                   reset                   esito
010400990921     C                   reset                   wrkesito
010500010601     C*
010600010601     C                   exsr      opeini
010700010605     C                   exsr      rwvab
010800010601     C*
010900010601     C                   seton                                        lr
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011500010601     C*--------------------------------------------------------
011600010601     C     PREELA        BEGSR
011700010601     C*
011800010601     C* SE OCCORRE SPEDIRE IN FILIALE
011900010601     C                   if        invfil <> *zeros and
012000010601     C                             flgGiro = '0'
012100010601     C*
012200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012300010601     C                   eval      flgGiro = '1'
012400010601     C*
012500010601     C                   endif
012600010601     C*
012700010601     C                   ENDSR
012800010601     C***
012900010601
013000010601
013100010601
013200010601
013300010601     C*--------------------------------------------------------
013400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013500010601     C*--------------------------------------------------------
013600010601     C     ENDELA        BEGSR
013700040119     C*
013800040119     C* Effettuo la chiamata al CLLE preposto
013900040119     C                   call(e)   'TITVVTC'
014000040510     C                   parm                    parccm
014100040119     C                   parm                    parmbr
014200040119     C                   parm      '2'           paropz
014300050201     C*
014400050201     C* Effettuo lancio TISI95 solo x chiusura
014500050201     C                   CLEAR                   TISI95DS
014600050201     C                   EVAL      I95TLA = 'C'
014700050201     C                   CALL      'TISI95R'
014800050201     C                   PARM                    TISI95DS
014900000616     C*
015000010601     C                   ENDSR
015100010601
015200010601
015300010330     C*--------------------------------------------------------
015400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015500010330     C*--------------------------------------------------------
015600010605     C     RWVAB         BEGSR
015700010330     C*
015800010330     C                   if        not %open(tivin00r)
015900010330     C                   open      tivin00r
016000010330     C                   endif
016100021025     C                   if        not %open(fivabwwr)
016200021025     C                   open      fivabwwr
016300010330     C                   endif
016400040119     C*
016500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016600040119     C                   exsr      prevat
016700040119     C*
016800040119     C                   if        chkcall = '0'
016900040119     C*
017000040119     C                   if        not %open(fivatwwr)
017100040119     C                   open      fivatwwr
017200040119     C                   endif
017300010330     C*
017400040119     C                   clear                   §CTROKVB          7 0
017500040119     C                   clear                   §CTROKVT          7 0
017600040119     C                   clear                   §CTRMO            7 0
017700040119     C                   clear                   §CTRNO            7 0
017800120410     C*
017900120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018000120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018100120410     C                   IF        Intestazioni = *off
018200120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018300120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018400120410     C                   exsr      inzvar
018500120410     C                   exsr      impvab
018600120410     C                   ENDIF
018700010330     C*
018800030822     C                   do        *HIVAL
018900010330     C*
019000030822     C                   read      tivin00r                               70
019100010618     C*
019200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019300010618     C                   if        vinflg = '1'
019400010618     C                   eval      flgOk = '1'
019500010618     C                   endif
019600040510     C*
019700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019800040510     C                   z-add     *zeros        wLenVINDTA        4 0
019900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020300040510     C                   z-add     1             wFlgVINDTA
020400040510     C                   leave
020500040510     C                   endif
020600040510     C                   enddo
020700010618     C*
020800040510     C                   if        vindta > *blanks AND
020900040510     C                             wFlgVINDTA = 1
021000010330     C                   add       1             rrnum
021100010330     C*
021200010601     C                   if        *in70 = *off and
021300010330     C                             (vinflg = *blanks
021400010330     C                              or vinflg = '0'
021500010330     C                              or vinflg = '2')
021600010330     C*
021700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021800010711     C                   if        vinflg = *blanks or vinflg = '0'
021900010711     C                   clear                   vinmsg
022000010711     C                   endif
022100010601     C*
022200070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022300070925     C                   eval      vindta  = ' ' + %trim(vindta)
022400070925     C*
022500080527     C                   exsr      inzvar
022600100318     C                   exsr      normCSV
022700010605     C                   exsr      impvab
022800010601     C*
022900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023000010605     C                   eval      depfil = VABLNP
023100010601     C                   exsr      repfil
023200010601     C                   if        depfil = invfil
023300021025     C                   if        vlrpoi = 999
023400030822     C                   move(P)   invfil        VABFGS
023500021025     C                   else
023600030822     C                   move(P)   vlrpoi        VABFGS
023700021025     C                   endif
023800010601     C*
023900010601     C                   exsr      PREELA
024000010601     C*
024100010604     C* Ebbene...
024200010604     C*
024300040119     C  N31              add       1             §CTROKVB
024400040119     C   32              add       1             §CTRMO
024500040119     C   31              add       1             §CTRNO
024600080418     C*
024700030822     C                   if        wGiro = 2
024800080513     C* Scrivo sempre il VAT
024900080513     C  N31              exsr      wrivat
025000110201     C                   exsr      chkvab
025100080513     C                   endif
025200010604     C*
025300010604     C                   if        *in31 = *off and
025400010604     C                             *in32 = *off
025500010604     C                   eval      vinflg = '1'
025600010604     C                   else
025700010604     C                   eval      vinflg = '2'
025800010604     C                   endif
025900010604     C                   endif
026000010601     C*
026100010604     C                   endif
026200010604     C*
026300010330     C                   else
026400010330     C                   eval      vinflg = '1'
026500010330     C                   endif
026600010601     C*
026700010601     C  N70              update    tivin000
026800010330     C*
026900030822     C  N70              enddo
027000040119     C*
027100080527     C* Verifico dati rimasti "in cannna"...
027200080527     C                   seton                                        65
027300110201     C                   exsr      chkvab
027400080527     C*
027500040119     C                   endif                                                  (endif - chkcall)
027600010601     C*
027700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027800010601     C                   if        cntNonEl = *zeros or
027900010601     C                             flgMulti = '0'
028000010330     C* Se non ci sono record con errori ...
028100010601     C                   if        §ctrno = 0 and
028200010604     C                             §ctrmo = 0 and
028300010601     C                             flgStato <> '2'
028400010330     C* ... restituisco esito OK.
028500010330     C                   eval      wrkesito = '0'
028600010330     C                   else
028700040119     C                   if        §ctrokvb > 0
028800010330     C                   eval      wrkesito = '1'
028900010330     C                   else
029000010615     C                   if        flgOk = '0'
029100010615     C                   eval      wrkesito = '2'
029200010615     C                   else
029300010615     C                   eval      wrkesito = '6'
029400010615     C                   endif
029500010330     C                   endif
029600010330     C                   endif
029700010601     C                   else
029800010601     C                   eval      wrkesito = '9'
029900010601     C                   endif
030000010330     C*
030100010330     C                   if        %open(tivin00r)
030200010330     C                   close     tivin00r
030300010330     C                   endif
030400021025     C                   if        %open(fivabwwr)
030500021025     C                   close     fivabwwr
030600010330     C                   endif
030700040512     C                   if        %open(fivatwwr)
030800040512     C                   close     fivatwwr
030900040512     C                   endif
031000010601     C*
031100010601     C                   if        vlrpoi <> 999
031200010601     C                   eval      invfil = vlrpoi
031300010601     C                   endif
031400010330     C*
031500040119     C                   if        §ctrokvb > 0
031600010601     C                             and invfil > *zeros
031700010330     C                   exsr      invio
031800010330     C                   endif
031900010601     C*
032000010618     C                   if        flgGiro = '1'
032100010601     C                   exsr      endela
032200010618     C                   endif
032300010330     C*
032400010330     C                   ENDSR
032500010330     C***
032600080527
032700080527
032800080527
032900080527     C*----------------------------------------------------*
033000080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033100080527     C*----------------------------------------------------*
033200100413     C     CHKVAB        BEGSR
033300110201     C*
033400110201     C* Se richiesti ignorare colli / peso / volume
033500110315 xxx C***                if        *in35
033600110315 xxx C***                z-add     *zeros        VABNCL
033700110315 xxx C***                z-add     *zeros        VABPKB
033800110315 xxx C***                z-add     *zeros        VABVLB
033900110315 xxx C***                endif
034000080527     C*
034100080527     C* Scrivo il VAB solo a rottura d spedizione
034200080527     C                   if        depspe = *zeros
034300080527     C  N31              eval      fivabds_sav = fivabds
034400080527     C                   eval      depspe = vabNSP
034500080527     C                   else
034600080527     C                   if        depspe = vabNSP AND *in65 = *off
034700100413     C   51              add       VABNCL        sav_VABNCL
034800100413     C   51              add       VABPKB        sav_VABPKB
034900100413     C   51              add       VABVLB        sav_VABVLB
035000100720     C   52              z-add     VABNCL        sav_VABNCL
035100100720     C   52              z-add     VABPKB        sav_VABPKB
035200100720     C   52              z-add     VABVLB        sav_VABVLB
035300100720     C   53              add       VABNCL        sav_VABNCL
035400100720     C   53              z-add     VABPKB        sav_VABPKB
035500100720     C   53              z-add     VABVLB        sav_VABVLB
035600080527     C                   else
035700080527     C  N31              eval      fivabds_ok = fivabds_sav
035800080527     C  N31              exsr      wrivab
035900080527     C  N31              eval      fivabds_sav = fivabds
036000080527     C                   eval      depspe = vabNSP
036100080527     C                   endif
036200080527     C                   endif
036300080527     C*
036400080527     C                   ENDSR
036500040119
036600040119
036700040119
036800040119     C*----------------------------------------------------*
036900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037000040119     C*----------------------------------------------------*
037100040510     C     WRIVAB        BEGSR
037200080513     C*
037300080513     C* Gestisco l'eventuale rottura x numero spedizione
037400080527     C                   if        VABNSP <> depspe or *in65 = *on
037500070301     C                   if        wVATNOT_E <> *blanks
037600070301     C                   eval      VABCTM = '7Q'
037700070301     C                   endif
037800100413     C                   write     fivab000
037900080513     C                   endif
038000040119     C*
038100040119     C                   ENDSR
038200040119
038300040119
038400040119
038500040119     C*----------------------------------------------------*
038600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038700040119     C*----------------------------------------------------*
038800040510     C     WRIVAT        BEGSR
038900060512     C*
039000060512     C* Inizializzo i campi chiave
039100060512     C                   eval      VATFGS = VABFGS
039200060512     C                   eval      VATAAS = VABAAS
039300060512     C                   eval      VATCCM = VABCCM
039400070823     C                   eval      VATNRS = VABNRS
039500060512     C                   eval      VATNSP = VABNSP
039600060512     C                   eval      VATLNP = VABLNP
039700040119     C*
039800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039900040510     C                   if        VATTRC = *blanks
040000040510     C                   eval      VATTRC = 'E'
040100040510     C                   endif
040200110318     C                   if        VATNOT <> *blanks
040300040510     C                   write     FIVAT000
040400040119     C                   add       1             §CTROKVT
040500110318     C                   endif
040600060331     C*
040700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040800060331     C                   if        wVATNOT_A <> *blanks
040900060331     C                   eval      VATNOT = wVATNOT_A
041000060331     C                   eval      VATTRC = 'A'
041100060331     C                   write     FIVAT000
041200060331     C                   add       1             §CTROKVT
041300060331     C                   endif
041400060331     C*
041500060331     C                   if        wVATNOT_B <> *blanks
041600060331     C                   eval      VATNOT = wVATNOT_B
041700060331     C                   eval      VATTRC = 'B'
041800060331     C                   write     FIVAT000
041900060331     C                   add       1             §CTROKVT
042000060331     C                   endif
042100060331     C*
042200120320     C*
042300120410     C* i rcd di FIVAT con tipo E sono 1 in quanto è una spedizione monocollo
042400060331     C                   if        wVATNOT_E <> *blanks
042500110201     C                   eval      VATNOT = wVATNOT_E
042600060331     C                   eval      VATTRC = 'E'
042700120411     C*
042800120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
042900120411     C*
043000120411     C     1             DO        VABNCL        wNumCollo         3 0
043100120411     C*** da analisi del POC il progressivo colli è max 3 char e deve ccontiene gli zeri non
043200120411     C*** significativi
043300120411     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
043400120411     C                                %editc(wNumCollo:'X')
043500120410     C                   write     FIVAT000
043600120410     C                   add       1             §CTROKVT
043700120411     C                   ENDDO
043800120320     C*
043900120320     C                   endif
044000120320     C*
044100110927     C*
044200110927     C                   if        wVATNOT_I <> *blanks
044300110927     C                   eval      VATNOT = wVATNOT_I
044400110927     C                   eval      VATTRC = 'I'
044500110927     C                   write     FIVAT000
044600110927     C                   add       1             §CTROKVT
044700110927     C                   endif
044800040119     C*
044900140226     C                   if        wVATNOT_J <> *blanks
045000140226     C                   eval      VATNOT = wVATNOT_J
045100140226     C                   eval      VATTRC = 'J'
045200140226     C                   write     FIVAT000
045300140226     C                   add       1             §CTROKVT
045400140226     C                   endif
045500140226     C*
045600040119     C                   ENDSR
045700040119
045800040119
045900040119
046000040119     C*----------------------------------------------------*
046100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046200040119     C*----------------------------------------------------*
046300040119     C     PREVAT        BEGSR
046400040119     C*
046500040119     C* Compongo il nome del membro da dare al FIVATWWR
046600040119     C                   eval      parmbr = vlrhdl
046700040119     C                   movel     'M'           parmbr
046800040510     C                   eval      parccm = vlrksc
046900040119     C                   eval      paropz = '1'
047000040119     C* Effettuo la chiamata al CLLE preposto
047100040119     C                   call(e)   'TITVVTC'
047200040119     C                   parm                    parccm
047300040119     C                   parm                    parmbr
047400040119     C                   parm                    paropz
047500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047600040119     C                   if        %error
047700040119     C                   movel     '1'           chkcall
047800040119     C                   else
047900040119     C                   movel     '0'           chkcall
048000040119     C                   endif
048100040119     C*
048200040119     C                   ENDSR
048300010601
048400010601
048500010601
048600010330     C*----------------------------------------------------*
048700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048800010330     C*----------------------------------------------------*
048900040119     C     INZVAR        BEGSR
049000080527     C*
049100080527     C                   setoff                                       65
049200010330     C*
049300030822     C* Inizializzo variabili di wrk
049400020204     C                   Z-ADD     *zeros        Num5_0            5 0
049500101018     C                   MOVEL     *blanks       wFlgCAS
049600060331     C                   MOVEL     *blanks       wVATNOT_A
049700060331     C                   MOVEL     *blanks       wVATNOT_B
049800060331     C                   MOVEL     *blanks       wVATNOT_E
049900110927     C                   MOVEL     *blanks       wVATNOT_I
050000140226     C                   MOVEL     *blanks       wVATNOT_J
050100140226     C                   MOVEL     *blanks       wVATNOT_IJ
050200120411     C                   MOVEL     *blanks       wNOT
050300120411     C                   MOVEL     *blanks       Data8A
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
081900110315 xxx C***                SETON                                        35
082000030515     C*
082100010607     C                   MOVEL     datcor        VABAAS
082200010607     C                   MOVE      datcor        VABMGS
082300010607     C*
082400030715     C**********
082500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082600030715     C**********
082700030715     C                   IF        wGiro = *zeros
082800040113     C*
082900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083200080429     C                             %trim(intcol(5))<>*blanks
083300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083500080429     C                                      %trim(intcol(5))
083600070823     C     ';':CharCSV   XLATE     vindta        vindta
083700040510     C                   ENDIF
083800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083900031201     C     minu:maiu     XLATE     vindta        vindta
084000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084100030715     C                   DOW       posDa <= %len(%trim(vindta))
084200030715     C*
084300030715     C* Gestisco il 1° campo
084400030715     C                   IF        i = 1
084500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084600030715     C                   EVAL      posDa = 1
084700030715     C* Eseguo lo scan x trovare la fine del primo campo
084800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C* X i campi successivi al 1°
085300030715     C                   ELSE
085400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085500030715     C                   EVAL      posDa = posA + 1
085600030715     C* Eseguo lo scan x trovare la fine del campo corrente
085700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085800030715     C* Gestisco l'ultimo campo
085900030715     C                   IF        posA = *zeros
086000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086100030715     C* A questo punto "estraggo" il campo corrente
086200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086300030715     C                                                    (posA-posDa))
086400030715     C                   LEAVE
086500030715     C                   ELSE
086600030715     C* A questo punto "estraggo" il campo corrente
086700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086800030715     C                                                    (posA-posDa))
086900030715     C                   ENDIF
087000030715     C                   ENDIF
087100030715     C* Incremento il contatore d campo
087200030715     C                   EVAL      i = i +1
087300030715     C                   ENDDO
087400030715     C*
087500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087600030715     C* delimitazione testo
087700030715     C                   EVAL      i = 1
087800030715     C                   DOW       i < %elem(SkSplitFLD)
087900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088100030715     C                   EVAL      i = i + 1
088200030715     C                   ENDDO
088300030715     C                   EVAL      wGiro = 1
088400030715     C                   ELSE
088500030715     C                   EVAL      wGiro = 2
088600031204     C**********
088700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088800031204     C* d separatore campo
088900031204     C**********
089000031204     C                   EVAL      vindta = %trim(vindta)
089100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089200031204     C                   EVAL      vindta = %subst(vindta:2)
089300031204     C                   ENDDO
089400031204     C*
089500031204     C                   Z-ADD     *zeros        lunghInput        4 0
089600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090000110912     C                   ENDDO
090100030715     C**********
090200030715     C* Effettuo lo split del campo dati d input
090300030715     C**********
090400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090500030715     C                   DOW       posDa <= %len(%trim(vindta))
090600030715     C*
090700030715     C* Gestisco il 1° campo
090800030715     C                   IF        i = 1
090900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091000030715     C                   EVAL      posDa = 1
091100030715     C* Eseguo lo scan x trovare la fine del primo campo
091200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C* X i campi successivi al 1°
091700030715     C                   ELSE
091800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091900030715     C                   EVAL      posDa = posA + 1
092000031204     C* Verifico che nn vi sia il campo nn valorizzato
092100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092200031204     C* Se campo nn valorizzato skippo al prossimo
092300031204     C                   EVAL      SkSplitCSV(i) = *blanks
092400031204     C                   EVAL      posA  = posA + 1
092500031204     C                   ELSE
092600030715     C* Eseguo lo scan x trovare la fine del campo corrente
092700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092800030715     C* Gestisco l'ultimo campo
092900030715     C                   IF        posA = *zeros
093000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093100030715     C* A questo punto "estraggo" il campo corrente
093200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093300030715     C                                                    (posA-posDa))
093400030715     C                   LEAVE
093500030715     C                   ELSE
093600030715     C* A questo punto "estraggo" il campo corrente
093700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093800030715     C                                                    (posA-posDa))
093900030715     C                   ENDIF
094000031204     C                   ENDIF
094100030715     C                   ENDIF
094200030715     C* Incremento il contatore d campo
094300030715     C                   EVAL      i = i +1
094400030715     C                   ENDDO
094500030715     C*
094600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094700030715     C* delimitazione testo
094800030715     C                   EVAL      i = 1
094900030715     C                   DOW       i < %elem(SkSplitCSV)
095000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095200030715     C                   EVAL      i = i + 1
095300030715     C                   ENDDO
095400100720     C*
095500100720     C* Stacco un progressivo x numero spedizione
095600120301 xxx C                   EXSR      repNSP
095700100326     C*
095800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095900030715     C                   EVAL      i = 1
096000030715     C                   DOW       i < %elem(SkSplitFLD)
096100030715     C*
096200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096400030715     C                             SkSplitCSV(i) <> *blanks
096500030721     C*** ATB
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096700030721     C                   EVAL      VABATB = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030715     C*** RSD
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097400030715     C* ==
097500030715     C                   ENDIF
097600030721     C*** RD2
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030715     C*** IND
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098200030715     C                   EVAL      VABIND = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** LOD
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** PRD
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030721     C*** NZD
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** RMA
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** NOT
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** NT2
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** VCA
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** VAS
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** FFD
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030721     C*** GC1
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** GC2
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** TSP
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103100030721     C                   ENDIF
103200040510     C*** CBO
103300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103500040510     C                   ENDIF
103600030721     C*** NAS
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** TIC
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** GCA
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** XCO
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** CTM
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** TCR
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** CTS
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** FTM
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** VAD
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GMA
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** GGA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** GVA
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TC1
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** TC2
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** SCL
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** RMO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** NMO
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*
110500030721     C* Reperisco quindi i campi numerici...
110600040510     C*** NSP
110700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110800040510     C                   EVAL      PiStr=SkSplitCSV(i)
110900040510     C                   EXSR      CHKNUM
111000040510     C                   IF        PiInt=*on
111100040510     C                   Z-ADD     PiVal         VABNSP
111200040510     C                   ELSE
111300040510     C                   SETON                                        31
111400040510     C                   EVAL      VABNSP = *zeros
111500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111600040510     C                             + ' ' + 'VABNSP'
111700040510     C                   ENDIF
111800040510     C                   ENDIF
111900071106     C*** RMN
112000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112100071106     C                   EVAL      PiStr=SkSplitCSV(i)
112200071106     C                   EXSR      CHKNUM
112300071106     C                   IF        PiInt=*on
112400071106     C                   Z-ADD     PiVal         VABRMN
112500071106     C                   ELSE
112600071106     C                   SETON                                        32
112700071106     C                   EVAL      VABRMN = *zeros
112800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112900071106     C                             + ' ' + 'VABRMN'
113000071106     C                   ENDIF
113100071106     C                   ENDIF
113200030721     C*** CAD
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on
113700030721     C                   Z-ADD     PiVal         Num5_0
113800030721     C                   MOVEL(p)  Num5_0        VABCAD
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      VABCAD = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300030721     C                             + ' ' + 'VABCAD'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** NCL
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on
115100030721     C                   Z-ADD     PiVal         VABNCL
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      VABNCL = *zeros
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600040122     C                             + ' ' + 'VABNCL'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** PKB
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiNum=*on
116400030721     C                   Z-ADD     PiVal         VABPKB
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABPKB = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900030721     C                             + ' ' + 'VABPKB'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** CAS
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
117500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
117600111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
117700120227 xxx C                   EVAL      VABvca = 'EUR'
117800101018     C                   EVAL      wFlgCAS = '1'
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
127900030721     C                   Z-ADD     PiVal         VABVLB
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
143100120411      *** W_VABTCR: mandano D=Dopo (come noi), P=Prima (come noi), I=Il ma noi usiamo ' '
143200120411     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTCR'
143300120411     C                   SELECT
143400120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'D'
143500120411     C                   EVAL      VABTCR='D'
143600120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'P'
143700120411     C                   EVAL      VABTCR='P'
143800120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'I'
143900120411     C                   EVAL      VABTCR=*blank
144000120411     C                   ENDSL
144100120411     C                   ENDIF
144200120411      *** W_VABDCR: la data è in formato gg/mm/aaaa alfanum. e va trasformata in 8 S
144300120411     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABDCR'
144400120517     C* il cliente può mandare la data con l'anno di 2 o di 4, bisogna gestirla
144500120517     C                   IF        %len(%trim(SkSplitCSV(i)))=10
144600120411     C                   EVAL      Data8A = %subst(%trim(SkSplitCSV(i)):7:4) +
144700120411     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
144800120411     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
144900120411     C                   MOVEL     Data8A        VABDCR
145000120517     C                   ELSE
145100120517     C* se la data non fosse né di 6 né di 8, valorizzo VABDCR come se mi arrivasse di 6
145200120517     C* così se il valore è impossibile va in errore e ce ne accorgiamo
145300120517     C                   EVAL      Data8A = %subst(%char(datcor):1:2) +
145400120517     C                                      %subst(%trim(SkSplitCSV(i)):7:2) +
145500120517     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
145600120517     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
145700120517     C                   MOVEL     Data8A        VABDCR
145800120517     C                   ENDIF
145900120517     C                   ENDIF
146000120411      *** W_VABCBO: F = 1, A = 2
146100120411     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
146200120411     C                   SELECT
146300120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'F'
146400120411     C                   EVAL      VABCBO='1'
146500120411     C                   WHEN      %trim(SkSplitCSV(i)) = 'A'
146600120411     C                   EVAL      VABCBO='2'
146700120411     C* se sbagliato valore imposto di default Porto Franco
146800120411     C                   OTHER
146900120411     C                   EVAL      VABCBO='1'
147000120411     C                   ENDSL
147100120411     C                   ENDIF
147200140226      *** wNOT è la concatenazione di WVABNT2_1/2
147300140227      *** ci sono 2 colonne nel .csv che si riferiscono a note di spedizione
147400120411      *** vengono tutte concatenate e alla fine suddivise tra VABNOT e VABNT2
147500140620      ****** per richiesta W_VABNT2_1 non c'è più
147600140116     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNT2_1' or
147700140226     C                             %trim(SkSplitFLD(i)) = 'W_VABNT2_2'
147800120411     C                   EVAL      wNOT=%trim(wNOT)+' '+%trim(SkSplitCSV(i))
147900120411     C                   ENDIF
148000140116     C*
148100140116      *** W_VABNOT
148200140116     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
148300140116     C                   IF        %scan('A':%trim(SkSplitCSV(i))) > 0
148400140116     C                   EVAL      VABTC1='A'
148500140116     C                   ENDIF
148600140116     C                   IF        %scan('P':%trim(SkSplitCSV(i))) > 0
148700140116     C                   EVAL      VABTC2='P'
148800140116     C                   ENDIF
148900140116     C                   IF        %scan('E':%trim(SkSplitCSV(i))) > 0
149000140116     C                   EVAL      VABTSP='E'
149100140116     C                   ENDIF
149200140116     C                   IF        %scan('H':%trim(SkSplitCSV(i))) > 0
149300140116     C                   EVAL      VABTSP='H'
149400140116     C                   ENDIF
149500140116     C                   ENDIF
149600140226
149700140226      *** W_VATNOT_I: è sia VATNOT_I che VATNOT_J
149800140226     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
149900140226     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
150000140226     C                   ENDIF
150100070925     C*
150200071029     C***  <===  -----------------------------------------
150300040510     C*
150400030715     C                   ENDIF
150500030715     C                   EVAL      i = i + 1
150600030715     C                   ENDDO
150700070925     C*
150800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
150900120411     C*
151000120411     C* wNOT è stato valorizzato concatenando più colonne
151100120411     C                   EVAL      VABNOT=%subst(wNOT:1:35)
151200120411     C                   EVAL      VABNT2=%subst(wNOT:36:35)
151300130618     C*
151400130618     C* se VABCCM è stato valorizzato con 0566080, allora:
151500140708     C                   IF        VABCCM = 0056080  and
151600130619     C                             wFlgCAS = '1'
151700130618     C                   EVAL      VABTIC='TO'
151800130618     C                   EVAL      VABRMO='ARTHEMIA SRL'
151900130618     C                   ENDIF
152000140708     C*
152100140708     C* se VABCCM è stato valorizzato con 0057610, allora:
152200140708     C                   IF        VABCCM = 0057610  and
152300140708     C                             wFlgCAS = '1'
152400140708     C                   EVAL      VABTIC='TM'
152500140708     C                   ENDIF
152600140226     C*
152700140226     C* email destinatario
152800140226     C                   IF        %trim(wVATNOT_IJ) <> *blanks
152900140226     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
153000140226     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
153100140226     C                   ENDIF
153200100831     C*
153300070925     C*
153400071029     C***  <===  -----------------------------------------
153500070925     C*
153600070925     C* Considerazioni finali su CBO/CAS
153700100318     C                   IF        wFlgCAS = '1'    OR
153800100318     C                             VABCAS  > *zeros
153900070925     C                   IF        VABCBO = '1'
154000070925     C                   EVAL      VABCBO = '4'
154100070925     C                   ENDIF
154200070925     C                   IF        VABCBO = '2'
154300070925     C                   EVAL      VABCBO = '6'
154400070925     C                   ENDIF
154500070925     C                   ENDIF
154600070925     C***  <===  ----------------------------
154700040510     C*
154800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154900040510     C                   IF        VABNSP = *zeros
155000040510     C                   SETON                                        31
155100040510     C                   ENDIF
155200040715     C*
155300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155400040715     C                   IF        VABLNP = *zeros
155500040715     C                   SETON                                        31
155600040715     C                   ENDIF
155700040510     C*
155800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155900120411     C                   IF        VABRMN = *zeros
156000120411     C                   Z-ADD     VABNSP        VABRMN
156100120411     C                   ENDIF
156200120328     C* IN QUESTO CASO LO FORZO = 1
156300120411     C***                IF        VABRMN = *zeros
156400120411     C***                Z-ADD     1             VABRMN
156500120411     C***                ENDIF
156600040510     C*
156700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156800040510     C                   IF        VABRMA = *blanks
156900040714     C                   MOVEL     VABRMN        VABRMA
157000040510     C                   ENDIF
157100040128     C*
157200040510     C* Se provincia nn valorizzata la reperisco
157300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
157400110912     C                   IF        VABPRD =  *blanks AND
157500110912     C                             VABNZD =  *blanks AND
157600110912     C                             VABCAD <> *blanks AND
157700110912     C                             VABLOD <> *blanks
157800040128     C                   CLEAR                   TISI95DS
157900040128     C                   EVAL      I95TCN = '3'
158000040128     C                   Z-ADD     datcor        I95DAT
158100040128     C                   EVAL      I95NAR = VABNZD
158200040128     C                   EVAL      I95CAP = VABCAD
158300040128     C                   EVAL      I95LOC = VABLOD
158400040128     C                   CALL      'TISI95R'
158500040128     C                   PARM                    TISI95DS
158600040128     C                   EVAL      VABPRD = O95PRV
158700040510     C                   ENDIF
158800020204     C*
158900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
159000020204     C                   EXSR      CHKIMPDIV
159100030715     C                   ENDIF
159200020204     C*
159300010607     C                   ENDSR
159400010607     C*----------------------------------------------------*
159500010601
159600020204
159700020204     C*----------------------------------------------------*
159800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159900020204     C*----------------------------------------------------*
160000020204     C     CHKIMPDIV     BEGSR
160100020204     C*
160200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
160300020204     C                   Z-ADD     *zeros        wrkDec            9 9
160400020204     C*
160500020204     C* Come prima cosa effettuo considerazioni sulla divisa
160600020204     C                   IF        vabIAS > *zeros
160700020204     C                   IF        vabVAS <> 'EUR'
160800020204     C                   EVAL      vabVAS =  'ITL'
160900020204     C                   ENDIF
161000020204     C                   ENDIF
161100020204     C*
161200020204     C                   IF        vabCAS > *zeros
161300020204     C                   IF        vabVCA <> 'EUR'
161400020204     C                   EVAL      vabVCA =  'ITL'
161500020204     C                   ENDIF
161600020204     C                   ENDIF
161700020204     C*
161800020204     C                   IF        vabVMD > *zeros
161900020204     C                   IF        vabVAD <> 'EUR'
162000020204     C                   EVAL      vabVAD =  'ITL'
162100020204     C                   ENDIF
162200020204     C                   ENDIF
162300020204     C*
162400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162500020204     C                   Z-ADD     vabIAS        wrkDec
162600020204     C                   IF        wrkDec > *zeros
162700020204     C                   IF        vabVAS = 'ITL'
162800020204     C                   EVAL      vabIAS = *zeros
162900020204     C                   ENDIF
163000020204     C                   ENDIF
163100020204     C*
163200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
163300020204     C                   Z-ADD     vabCAS        wrkDec
163400020204     C                   IF        wrkDec > *zeros
163500020204     C                   IF        vabVCA = 'ITL'
163600020204     C                   EVAL      vabCAS = *zeros
163700020204     C                   ENDIF
163800020204     C                   ENDIF
163900020204     C*
164000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
164100020204     C                   Z-ADD     vabVMD        wrkDec
164200020204     C                   IF        wrkDec > *zeros
164300020204     C                   IF        vabVAD = 'ITL'
164400020204     C                   EVAL      vabVMD = *zeros
164500020204     C                   ENDIF
164600020204     C                   ENDIF
164700020204     C*
164800020204     C                   ENDSR
164900020204     C***
165000010330
165100100318
165200100318
165300100318     C*----------------------------------------------------*
165400100318     C*  CONTROLLO NUMERICITA' CAMPI
165500100318     C*----------------------------------------------------*
165600100318     C     CHKNUM        BEGSR
165700100318     C*
165800100318     C                   IF        PiDecChr = *blanks
165900100318     C                   EVAL      PiDecChr = CharNUM
166000100318     C                   ENDIF
166100100318     C*
166200100318     C                   callp     UBISNUM_Check(PiStr
166300100318     C                                          :PiDecChr
166400100318     C                                          :PiVal
166500100318     C                                          :PiNum
166600100318     C                                          :PiInt)
166700100318     C*
166800100318     C                   ENDSR
166900100318     C***
167000010601
167100010601
167200010601
167300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
167400010601     C     repfil        BEGSR
167500010601     C*
167600010601     C                   if        invfil = *zeros and
167700010601     C                             depfil > *zeros and
167800010629     C                             (vinflg = *blanks or
167900010629     C                              vinflg = *zeros)
168000010601     C
168100010601     C                   eval      invfil = depfil
168200010601     C                   endif
168300010601     C*
168400010601     C                   if        depfil <> invfil and
168500010601     C                             invfil > *zeros
168600010601     C                   eval      flgMulti = '1'
168700010601     C                   if        vinflg = *blanks
168800010601     C                   add       1             cntNonEl
168900010601     C                   endif
169000010601     C                   endif
169100010601     C*
169200010601     C                   if        vinflg = '2'
169300010601     C                   eval      flgStato = '2'
169400010601     C                   endif
169500010601     C*
169600010601     C                   ENDSR
169700010601     C***
169800010601
169900010601
170000010601
170100010330
170200040119      /TITLE Invio dei dati al punto operativo.
170300040119     C     invio         BEGSR
170400040119     C*
170500040119     C* 1° invio FIVAT
170600040119     C                   reset                   dscmz
170700040119     C                   move      vlrpoi        cmzdst
170800040119     C                   eval      cmzfld = 'FIVATWWR'
170900040119     C                   eval      cmzmbd = vlrhdl
171000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171100040119     C***                if        prmfir = *blanks
171200040119     C                   eval      cmzfla = 'FIVAT00F'
171300040119     C                   eval      cmzmba = 'FIVAT00F'
171400040119     C***                else
171500040119     C***                eval      cmzfla = prmfir
171600040119     C***                eval      cmzmba = prmfir
171700040119     C***                endif
171800040119     C                   eval      cmznrr = *zeros
171900040119     C                   move      §ctrokvt      cmznrr
172000040119     C                   eval      cmzlba = vlrfl1
172100040119     C                   call(e)   'TIS711C'
172200040119     C                   parm                    dscmz
172300040119     C                   parm      *blanks       esito
172400040119     C                   if        %error
172500040119     C                             or cmzerr = '1'
172600040119     C                             or esito  = '1'
172700040119     C                   eval      wrkesito = '3'
172800040119     C                   else
172900040119     C*
173000040119     C* 2° invio FIVAB
173100040119     C                   reset                   dscmz
173200040119     C                   move      vlrpoi        cmzdst
173300040119     C                   eval      cmzfld = vlrfou
173400040119     C                   eval      cmzmbd = vlrhdl
173500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173600040119     C***                if        prmfir = *blanks
173700040119     C                   eval      cmzfla = 'FIVAB00F'
173800040119     C                   eval      cmzmba = 'FIVAB00F'
173900040119     C***                else
174000040119     C***                eval      cmzfla = prmfir
174100040119     C***                eval      cmzmba = prmfir
174200040119     C***                endif
174300040119     C                   eval      cmznrr = *zeros
174400040119     C                   move      §ctrokvb      cmznrr
174500040119     C                   eval      cmzlba = vlrfl1
174600040119     C                   call(e)   'TIS711C'
174700040119     C                   parm                    dscmz
174800040119     C                   parm      *blanks       esito
174900040119     C                   if        %error
175000040119     C                             or cmzerr = '1'
175100040119     C                             or esito  = '1'
175200040119     C                   eval      wrkesito = '3'
175300040119     C                   endif
175400040119     C                   endif
175500040119     C*
175600040119     C                   ENDSR
175700040119     C***
175800010601
175900010601
176000010601
176100010601
176200010601      /TITLE Invio dei dati al punto operativo.
176300010601     C     opeini        BEGSR
176400010601     C*
176500010601     C* Inizializzo flag e contatori operativi
176600010601     C                   movel     '0'           flgGiro           1
176700010601     C                   movel     '0'           flgMulti          1
176800010601     C                   movel     '1'           flgStato          1
176900010615     C                   movel     '0'           flgOk             1
177000010601     C                   z-add     *zeros        cntNonEl         10 0
177100010601     C                   z-add     *zeros        depfil            3 0
177200010601     C                   z-add     *zeros        invfil            3 0
177300010601     C*
177400010601     C                   ENDSR
177500010601     C***
177600070326
177700070326
177800070326
177900070326
178000070326     C     *pssr         BEGSR
178100070329     C*
178200070329     C                   if        %open(tivin00r)
178300070329     C                   close     tivin00r
178400070329     C                   endif
178500070329     C                   if        %open(fivabwwr)
178600070329     C                   close     fivabwwr
178700070329     C                   endif
178800070329     C                   if        %open(fivatwwr)
178900070329     C                   close     fivatwwr
179000070329     C                   endif
179100070326     C*
179200070326     C* Effettuo la chiamata al CLLE preposto
179300070326     C                   call(e)   'TITVVTC'
179400070326     C                   parm                    parccm
179500070326     C                   parm                    parmbr
179600070326     C                   parm      '2'           paropz
179700070326     C*
179800070326     C                   eval      wrkesito = '2'
179900070404     C*
180000070404     C                   seton                                        LR
180100070326     C*
180200070326     C                   ENDSR     '*CANCL'
180300070326     C***
180400100318
180500100318
180600100318
180700100318     C     normCSV       BEGSR
180800100318     C*
180900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
181000100318     C* separatori campi
181100100318     C                   if        CharTXT <> *blanks
181200100318     C                   z-add     *zeros        wLenVINDTA        4 0
181300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
181400100318     C                   setoff                                       80
181500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
181600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181800100318     C                   if        not *in80
181900100318     C                   seton                                        80
182000100318     C                   else
182100100318     C                   setoff                                       80
182200100318     C                   endif
182300100318     C                   endif
182400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
182500100318     C                             and *in80 = *on
182600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182700100318     C                   endif
182800100318     C                   enddo
182900100318     C                   endif
183000100318     C*
183100100318     C                   ENDSR
183200100318     C***
183300070326
183400100326
183500100326
183600100326     C*----------------------------------------------------*
183700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183800100326     C*----------------------------------------------------*
183900100326     C     REPNSP        BEGSR
184000100326     C*
184100100326     C* NSP => Stacco un numeratore da AZNUM
184200100326     C                   clear                   TRUL33DS
184300100326     C                   eval      I33OPE = *zeros
184400100326     C                   eval      I33CNU = 302
184500100326     C                   eval      I33NUM = 1
184600100326     C                   movel     TRUL33DS      KPJBU
184700100326     C                   call      'TRUL33R'
184800100326     C                   parm                    KPJBA
184900100326     C                   movel     KPJBU         TRUL33DS
185000100326     C                   if        O33ERR = *zeros
185100100326     C                   z-add     O33NRF        VABNSP
185200100326     C                   z-add     O33NRF        VATNSP
185300100326     C                   else
185400100326     C                   SETON                                        31
185500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
185600100326     C                             + ' ' + 'VABNSP VATNSP'
185700100326     C                   endif
185800100326     C*
185900100326     C                   ENDSR
186000010330
186100100326
186200010330
186300000613     C     *inzsr        BEGSR
186400990910     C*
186500990910     C     *entry        plist
186600990920     C                   parm                    tivlrds
186700990921     C                   parm      wrkesito      esito
186800000724     C                   parm                    prmlit
186900000710     C                   parm                    prmfir
187000010330     C*
187100010330     C* CALCOLA LA DATA CORRENTE
187200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
187300100318     C                   eval      datcor = %dec(%date() : *ISO)
187400000613     C*
187500000613     C                   ENDSR
187600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187700120411VABNSP;X;VATNOT_E;VABNCL;X;VABRSD;VABRD2;VABIND;X;VABLOD;VABCAD;VABPRD;VATNOT_A;VATNOT_B;
187800120411W_VABTCR;W_VABDCR;W_VABNOT;VABRMO;VABCCM;W_VABCBO;X;X;X;X;X;X;VABPKB;X;VABIAS;X;VABCAS;
187900140620W_VATNOT_I;W_VABNT2_2;X;
188000111116
188100100318
