000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400140310      * il loro nr.DDT è sia VABNSP sia VABRMN sia VABNSP,
000500140310      *  i numerici cono solo gli ultimi char a dx numerici
000600140310      *  mentre RMA è la composizione dei primi 6 char + dall'11° in poi
000700130528      * il peso sarà sempre 0
000800130626      * Se la nazione che mi è stata passata è I o IT la pulisco, altrimenti non calcola la prov.
000900130528      *
001000121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001100120209     F*
001200990910     Ftivin00r  uF   E             DISK    usropn
001300100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001400040119     FFIVATwwr  O    E             DISK    usropn
001500000313     D*
001600040113     D*------------
001700040113     D* SCHIEDE A PROGRAMMA
001800040113     D*------------
001900080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700040128     D tisi95ds      e ds
002800100413     D fivabds       e ds                  extname(fivab00f)
002900100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003000100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600040510     D parccm          s              8    INZ(*blanks)
003700040119     D parmbr          s             10    INZ(*blanks)
003800040119     D paropz          s              1    INZ(*blanks)
003900040119     D chkcall         s              1    INZ(*blanks)
004000080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100080418     D wFlgCAS         s              1    INZ(*blanks)
004200120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004300120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004400130528     D Intestazioni    s               n   inz(*off)
004500100326     D*------------------
004600100326     D* DS REPERIMENTO NUMERATORE
004700100326     D*------------------
004800100326     D trul33ds      e ds                  inz
004900100326     D*------------------
005000100326     D* DS ARCHITETTURA
005100100326     D*------------------
005200100326     D kpjba         e ds                  inz
005300100326
005400030715     D*------------------
005500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600030715     D*------------------
005700030715     D SkSplitFLD      S             10    DIM(100)
005800030715     D SkSplitCSV      S            256    DIM(100)
005900030715     D CharCSV         S              1
006000030715     D CharTXT         S              1
006100030715     D CharNUM         S              1
006200100318     D CharSOS         S              1
006300030715     D posDa           S              3  0 INZ(*zeros)
006400030715     D posA            S              3  0 INZ(*zeros)
006500030715     D i               s              3  0 INZ(1)
006600030715     D wGiro           s              1  0 INZ(*zeros)
006700070823     D wVATNOT_A       s                   LIKE(VATNOT)
006800060331     D wVATNOT_B       s                   LIKE(VATNOT)
006900060331     D wVATNOT_E       s                   LIKE(VATNOT)
007000110927     D wVATNOT_I       s                   LIKE(VATNOT)
007100120411     D wNOT            s             70
007200120411     D Data8A          s              8
007300130625     D w50             s             50
007400130625     D wX              s              3  0
007500130625     D num             c                   const('1234567890 ')
007600030822     D*------------------
007700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007800030822     D*------------------
007900030822     D posDaDft        S              3  0 INZ(*zeros)
008000030822     D posADft         S              3  0 INZ(*zeros)
008100030822     D j               s              3  0 INZ(1)
008200030822     D wGiroDft        s              1  0 INZ(*zeros)
008300080418     D*------------------
008400080418     D* Costanti
008500080418     D*------------------
008600080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008700080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
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
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700021025     C                   if        not %open(fivabwwr)
015800021025     C                   open      fivabwwr
015900010330     C                   endif
016000040119     C*
016100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016200040119     C                   exsr      prevat
016300040119     C*
016400040119     C                   if        chkcall = '0'
016500040119     C*
016600040119     C                   if        not %open(fivatwwr)
016700040119     C                   open      fivatwwr
016800040119     C                   endif
016900010330     C*
017000040119     C                   clear                   §CTROKVB          7 0
017100040119     C                   clear                   §CTROKVT          7 0
017200040119     C                   clear                   §CTRMO            7 0
017300040119     C                   clear                   §CTRNO            7 0
017400120410     C*
017500120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017600120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017700120410     C                   IF        Intestazioni = *off
017800120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017900120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018000120410     C                   exsr      inzvar
018100120410     C                   exsr      impvab
018200120410     C                   ENDIF
018300010330     C*
018400030822     C                   do        *HIVAL
018500010330     C*
018600030822     C                   read      tivin00r                               70
018700010618     C*
018800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018900010618     C                   if        vinflg = '1'
019000010618     C                   eval      flgOk = '1'
019100010618     C                   endif
019200040510     C*
019300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019400040510     C                   z-add     *zeros        wLenVINDTA        4 0
019500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019900040510     C                   z-add     1             wFlgVINDTA
020000040510     C                   leave
020100040510     C                   endif
020200040510     C                   enddo
020300010618     C*
020400040510     C                   if        vindta > *blanks AND
020500040510     C                             wFlgVINDTA = 1
020600010330     C                   add       1             rrnum
020700010330     C*
020800010601     C                   if        *in70 = *off and
020900010330     C                             (vinflg = *blanks
021000010330     C                              or vinflg = '0'
021100010330     C                              or vinflg = '2')
021200010330     C*
021300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021400010711     C                   if        vinflg = *blanks or vinflg = '0'
021500010711     C                   clear                   vinmsg
021600010711     C                   endif
021700010601     C*
021800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021900130605     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022000130605     C* evitare che vengano elisi i blank
022100130605     C                   eval      vindta  = '*' + %trim(vindta)
022200070925     C*
022300080527     C                   exsr      inzvar
022400100318     C                   exsr      normCSV
022500010605     C                   exsr      impvab
022600010601     C*
022700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022800010605     C                   eval      depfil = VABLNP
022900010601     C                   exsr      repfil
023000010601     C                   if        depfil = invfil
023100021025     C                   if        vlrpoi = 999
023200030822     C                   move(P)   invfil        VABFGS
023300021025     C                   else
023400030822     C                   move(P)   vlrpoi        VABFGS
023500021025     C                   endif
023600010601     C*
023700010601     C                   exsr      PREELA
023800010601     C*
023900010604     C* Ebbene...
024000010604     C*
024100040119     C  N31              add       1             §CTROKVB
024200040119     C   32              add       1             §CTRMO
024300040119     C   31              add       1             §CTRNO
024400080418     C*
024500030822     C                   if        wGiro = 2
024600080513     C* Scrivo sempre il VAT
024700080513     C  N31              exsr      wrivat
024800110201     C                   exsr      chkvab
024900080513     C                   endif
025000010604     C*
025100010604     C                   if        *in31 = *off and
025200010604     C                             *in32 = *off
025300010604     C                   eval      vinflg = '1'
025400010604     C                   else
025500010604     C                   eval      vinflg = '2'
025600010604     C                   endif
025700010604     C                   endif
025800010601     C*
025900010604     C                   endif
026000010604     C*
026100010330     C                   else
026200010330     C                   eval      vinflg = '1'
026300010330     C                   endif
026400010601     C*
026500010601     C  N70              update    tivin000
026600010330     C*
026700030822     C  N70              enddo
026800040119     C*
026900080527     C* Verifico dati rimasti "in cannna"...
027000080527     C                   seton                                        65
027100110201     C                   exsr      chkvab
027200080527     C*
027300040119     C                   endif                                                  (endif - chkcall)
027400010601     C*
027500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027600010601     C                   if        cntNonEl = *zeros or
027700010601     C                             flgMulti = '0'
027800010330     C* Se non ci sono record con errori ...
027900010601     C                   if        §ctrno = 0 and
028000010604     C                             §ctrmo = 0 and
028100010601     C                             flgStato <> '2'
028200010330     C* ... restituisco esito OK.
028300010330     C                   eval      wrkesito = '0'
028400010330     C                   else
028500040119     C                   if        §ctrokvb > 0
028600010330     C                   eval      wrkesito = '1'
028700010330     C                   else
028800010615     C                   if        flgOk = '0'
028900010615     C                   eval      wrkesito = '2'
029000010615     C                   else
029100010615     C                   eval      wrkesito = '6'
029200010615     C                   endif
029300010330     C                   endif
029400010330     C                   endif
029500010601     C                   else
029600010601     C                   eval      wrkesito = '9'
029700010601     C                   endif
029800010330     C*
029900010330     C                   if        %open(tivin00r)
030000010330     C                   close     tivin00r
030100010330     C                   endif
030200021025     C                   if        %open(fivabwwr)
030300021025     C                   close     fivabwwr
030400010330     C                   endif
030500040512     C                   if        %open(fivatwwr)
030600040512     C                   close     fivatwwr
030700040512     C                   endif
030800010601     C*
030900010601     C                   if        vlrpoi <> 999
031000010601     C                   eval      invfil = vlrpoi
031100010601     C                   endif
031200010330     C*
031300040119     C                   if        §ctrokvb > 0
031400010601     C                             and invfil > *zeros
031500010330     C                   exsr      invio
031600010330     C                   endif
031700010601     C*
031800010618     C                   if        flgGiro = '1'
031900010601     C                   exsr      endela
032000010618     C                   endif
032100010330     C*
032200010330     C                   ENDSR
032300010330     C***
032400080527
032500080527
032600080527
032700080527     C*----------------------------------------------------*
032800080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032900080527     C*----------------------------------------------------*
033000100413     C     CHKVAB        BEGSR
033100110201     C*
033200110201     C* Se richiesti ignorare colli / peso / volume
033300110315 xxx C***                if        *in35
033400110315 xxx C***                z-add     *zeros        VABNCL
033500110315 xxx C***                z-add     *zeros        VABPKB
033600110315 xxx C***                z-add     *zeros        VABVLB
033700110315 xxx C***                endif
033800080527     C*
033900080527     C* Scrivo il VAB solo a rottura d spedizione
034000080527     C                   if        depspe = *zeros
034100080527     C  N31              eval      fivabds_sav = fivabds
034200080527     C                   eval      depspe = vabNSP
034300080527     C                   else
034400080527     C                   if        depspe = vabNSP AND *in65 = *off
034500100413     C   51              add       VABNCL        sav_VABNCL
034600100413     C   51              add       VABPKB        sav_VABPKB
034700100413     C   51              add       VABVLB        sav_VABVLB
034800100720     C   52              z-add     VABNCL        sav_VABNCL
034900100720     C   52              z-add     VABPKB        sav_VABPKB
035000100720     C   52              z-add     VABVLB        sav_VABVLB
035100100720     C   53              add       VABNCL        sav_VABNCL
035200100720     C   53              z-add     VABPKB        sav_VABPKB
035300100720     C   53              z-add     VABVLB        sav_VABVLB
035400080527     C                   else
035500080527     C  N31              eval      fivabds_ok = fivabds_sav
035600080527     C  N31              exsr      wrivab
035700080527     C  N31              eval      fivabds_sav = fivabds
035800080527     C                   eval      depspe = vabNSP
035900080527     C                   endif
036000080527     C                   endif
036100080527     C*
036200080527     C                   ENDSR
036300040119
036400040119
036500040119
036600040119     C*----------------------------------------------------*
036700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036800040119     C*----------------------------------------------------*
036900040510     C     WRIVAB        BEGSR
037000080513     C*
037100080513     C* Gestisco l'eventuale rottura x numero spedizione
037200080527     C                   if        VABNSP <> depspe or *in65 = *on
037300070301     C                   if        wVATNOT_E <> *blanks
037400070301     C                   eval      VABCTM = '7Q'
037500070301     C                   endif
037600100413     C                   write     fivab000
037700080513     C                   endif
037800040119     C*
037900040119     C                   ENDSR
038000040119
038100040119
038200040119
038300040119     C*----------------------------------------------------*
038400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038500040119     C*----------------------------------------------------*
038600040510     C     WRIVAT        BEGSR
038700060512     C*
038800060512     C* Inizializzo i campi chiave
038900060512     C                   eval      VATFGS = VABFGS
039000060512     C                   eval      VATAAS = VABAAS
039100060512     C                   eval      VATCCM = VABCCM
039200070823     C                   eval      VATNRS = VABNRS
039300060512     C                   eval      VATNSP = VABNSP
039400060512     C                   eval      VATLNP = VABLNP
039500040119     C*
039600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039700040510     C                   if        VATTRC = *blanks
039800040510     C                   eval      VATTRC = 'E'
039900040510     C                   endif
040000110318     C                   if        VATNOT <> *blanks
040100040510     C                   write     FIVAT000
040200040119     C                   add       1             §CTROKVT
040300110318     C                   endif
040400060331     C*
040500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040600060331     C                   if        wVATNOT_A <> *blanks
040700060331     C                   eval      VATNOT = wVATNOT_A
040800060331     C                   eval      VATTRC = 'A'
040900060331     C                   write     FIVAT000
041000060331     C                   add       1             §CTROKVT
041100060331     C                   endif
041200060331     C*
041300060331     C                   if        wVATNOT_B <> *blanks
041400060331     C                   eval      VATNOT = wVATNOT_B
041500060331     C                   eval      VATTRC = 'B'
041600060331     C                   write     FIVAT000
041700060331     C                   add       1             §CTROKVT
041800060331     C                   endif
041900060331     C*
042000120320     C*
042100120410     C* i rcd di FIVAT con tipo E sono 1 in quanto è una spedizione monocollo
042200060331     C                   if        wVATNOT_E <> *blanks
042300110201     C                   eval      VATNOT = wVATNOT_E
042400060331     C                   eval      VATTRC = 'E'
042500120411     C*
042600120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
042700120411     C*
042800120411     C     1             DO        VABNCL        wNumCollo         3 0
042900120411     C*** da analisi del POC il progressivo colli è max 3 char e deve ccontiene gli zeri non
043000120411     C*** significativi
043100120411     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
043200120411     C                                %editc(wNumCollo:'X')
043300120410     C                   write     FIVAT000
043400120410     C                   add       1             §CTROKVT
043500120411     C                   ENDDO
043600120320     C*
043700120320     C                   endif
043800120320     C*
043900110927     C*
044000110927     C                   if        wVATNOT_I <> *blanks
044100110927     C                   eval      VATNOT = wVATNOT_I
044200110927     C                   eval      VATTRC = 'I'
044300110927     C                   write     FIVAT000
044400110927     C                   add       1             §CTROKVT
044500110927     C                   endif
044600040119     C*
044700040119     C                   ENDSR
044800040119
044900040119
045000040119
045100040119     C*----------------------------------------------------*
045200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045300040119     C*----------------------------------------------------*
045400040119     C     PREVAT        BEGSR
045500040119     C*
045600040119     C* Compongo il nome del membro da dare al FIVATWWR
045700040119     C                   eval      parmbr = vlrhdl
045800040119     C                   movel     'M'           parmbr
045900040510     C                   eval      parccm = vlrksc
046000040119     C                   eval      paropz = '1'
046100040119     C* Effettuo la chiamata al CLLE preposto
046200040119     C                   call(e)   'TITVVTC'
046300040119     C                   parm                    parccm
046400040119     C                   parm                    parmbr
046500040119     C                   parm                    paropz
046600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046700040119     C                   if        %error
046800040119     C                   movel     '1'           chkcall
046900040119     C                   else
047000040119     C                   movel     '0'           chkcall
047100040119     C                   endif
047200040119     C*
047300040119     C                   ENDSR
047400010601
047500010601
047600010601
047700010330     C*----------------------------------------------------*
047800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047900010330     C*----------------------------------------------------*
048000040119     C     INZVAR        BEGSR
048100080527     C*
048200080527     C                   setoff                                       65
048300010330     C*
048400030822     C* Inizializzo variabili di wrk
048500020204     C                   Z-ADD     *zeros        Num5_0            5 0
048600101018     C                   MOVEL     *blanks       wFlgCAS
048700060331     C                   MOVEL     *blanks       wVATNOT_A
048800060331     C                   MOVEL     *blanks       wVATNOT_B
048900060331     C                   MOVEL     *blanks       wVATNOT_E
049000110927     C                   MOVEL     *blanks       wVATNOT_I
049100120411     C                   MOVEL     *blanks       wNOT
049200120411     C                   MOVEL     *blanks       Data8A
049300030822     C*
049400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049500030822     C                   CLEAR                   SkSplitCSV
049600030822     C                   EVAL      i = 1
049700030822     C                   EVAL      posDa = *zeros
049800030822     C                   EVAL      posA  = *zeros
049900080527     C*
050000080527     C                   CLEAR                   FIVABDS
050100080527     C                   CLEAR                   FIVAT000
050200030822     C*
050300030822     C* Reimposto i valori di default
050400030822     C                   EXSR      DEFCAM
050500030822     C*
050600010330     C                   ENDSR
050700010330     C*----------------------------------------------------*
050800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050900010330     C*----------------------------------------------------*
051000010330     C     DEFCAM        BEGSR
051100030822     C*
051200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051300030715     C* e delimitatore testo.
051400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051700100318     C*
051800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051900100318     C                   EVAL      CharSOS = CharNUM
052000030822     C*
052100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052200100720     C                   SETOFF                                       505152
052300100720     C                   SETOFF                                       53
052400100413     C                   SELECT
052500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052600100720     C                   SETON                                        50
052700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052800100413     C                   SETON                                        51
052900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053000100720     C                   SETON                                        52
053100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053200100720     C                   SETON                                        53
053300100413     C                   ENDSL
053400030822     C*
053500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053600030822     C                   EVAL      posDaDft = 1
053700030822     C                   EVAL      posADft  = 0
053800030822     C                   EVAL      wGiroDft = 0
053900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054000030822     C                             posDaDft > 0
054100030822     C*
054200030822     C* Gestisco il 1° giro
054300030822     C                   IF        wGiroDft = 0
054400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054600030822     C* Incremento il contatore dei "giri"
054700030822     C                   EVAL      wGiroDft = 1
054800030822     C                   ELSE
054900030822     C                   EVAL      posDaDft = posADft
055000030822     C                   ENDIF
055100030822     C* Eseguo lo scan x trovare la fine del campo corrente
055200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055300030822     C*
055400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055600030822     C                   IF        posDaDft > 0 AND
055700030822     C                             posADft  > 0
055800030822     C* NCL
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'NCL'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiInt=*on
056800030822     C                   Z-ADD     PiVal         VABNCL
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* CCM
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'CCM'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiInt=*on
058100030822     C                   Z-ADD     PiVal         VABCCM
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* LNP
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'LNP'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiInt=*on
059400030822     C                   Z-ADD     PiVal         VABLNP
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* NRS
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'NRS'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABNRS
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* CTR
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'CTR'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiInt=*on
062000030822     C                   Z-ADD     PiVal         VABCTR
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* PKB
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'PKB'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiNum=*on
063300030822     C                   Z-ADD     PiVal         VABPKB
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* VLB
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'VLB'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiNum=*on
064600030822     C                   Z-ADD     PiVal         VABVLB
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* QFT
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'QFT'
065400030822     C                   EVAL      PiStr=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   EXSR      CHKNUM
065800030822     C                   IF        PiNum=*on
065900030822     C                   Z-ADD     PiVal         VABQFT
066000030822     C                   ENDIF
066100030822     C                   ENDIF
066200030822     C* CBO
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'CBO'
066700030822     C                   EVAL      VABCBO=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100030822     C* TSP
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'TSP'
067600030822     C                   EVAL      VABTSP=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* VAS
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'VAS'
068500030822     C                   EVAL      VABVAS=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* VCA
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'VCA'
069400030822     C                   EVAL      VABVCA=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800070925     C* TIC
069900070925     C                   IF        %subst(
070000070925     C                             %subst(vlrppt:posDaDft+1:
070100070925     C                             posADft-posDaDft-1):1:3)
070200070925     C                             = 'TIC'
070300070925     C                   EVAL      VABTIC=%trim(%subst(
070400070925     C                             %subst(vlrppt:posDaDft+1:
070500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600070925     C                   ENDIF
070700030822     C* GCA
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'GCA'
071200030822     C                   EVAL      VABGCA=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* CTM
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'CTM'
072100030822     C                   EVAL      VABCTM=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* FFD
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'FFD'
073000030822     C                   EVAL      VABFFD=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* VAD
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'VAD'
073900030822     C                   EVAL      VABVAD=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* GMA
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'GMA'
074800030822     C                   EVAL      VABGMA=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* GGA
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'GGA'
075700030822     C                   EVAL      VABGGA=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* GVA
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'GVA'
076600030822     C                   EVAL      VABGVA=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* TC1
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'TC1'
077500030822     C                   EVAL      VABTC1=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* TC2
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'TC2'
078400030822     C                   EVAL      VABTC2=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800040714     C* VATTRC
078900040714     C                   IF        %subst(
079000040714     C                             %subst(vlrppt:posDaDft+1:
079100040714     C                             posADft-posDaDft-1):1:3)
079200040714     C                             = 'TRC'
079300040714     C                   EVAL      VATTRC=%trim(%subst(
079400040714     C                             %subst(vlrppt:posDaDft+1:
079500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600040714     C                   ENDIF
079700030822     C* ...
079800030822     C                   ENDIF
079900030822     C                   ENDDO
080000020204     C*
080100010330     C                   ENDSR
080200010607     C*----------------------------------------------------*
080300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080400010607     C*----------------------------------------------------*
080500010607     C     IMPVAB        BEGSR
080600010607     C*
080700010607     C                   SETOFF                                       3132
080800110315 xxx C***                SETON                                        35
080900030515     C*
081000010607     C                   MOVEL     datcor        VABAAS
081100010607     C                   MOVE      datcor        VABMGS
081200010607     C*
081300030715     C**********
081400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081500030715     C**********
081600030715     C                   IF        wGiro = *zeros
081700040113     C*
081800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
082100080429     C                             %trim(intcol(5))<>*blanks
082200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082400080429     C                                      %trim(intcol(5))
082500070823     C     ';':CharCSV   XLATE     vindta        vindta
082600040510     C                   ENDIF
082700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082800031201     C     minu:maiu     XLATE     vindta        vindta
082900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083000030715     C                   DOW       posDa <= %len(%trim(vindta))
083100030715     C*
083200030715     C* Gestisco il 1° campo
083300030715     C                   IF        i = 1
083400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083500030715     C                   EVAL      posDa = 1
083600030715     C* Eseguo lo scan x trovare la fine del primo campo
083700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083800030715     C* A questo punto "estraggo" il campo corrente
083900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084000030715     C                                                    (posA-posDa))
084100030715     C* X i campi successivi al 1°
084200030715     C                   ELSE
084300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084400030715     C                   EVAL      posDa = posA + 1
084500030715     C* Eseguo lo scan x trovare la fine del campo corrente
084600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084700030715     C* Gestisco l'ultimo campo
084800030715     C                   IF        posA = *zeros
084900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085000030715     C* A questo punto "estraggo" il campo corrente
085100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085200030715     C                                                    (posA-posDa))
085300030715     C                   LEAVE
085400030715     C                   ELSE
085500030715     C* A questo punto "estraggo" il campo corrente
085600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085700030715     C                                                    (posA-posDa))
085800030715     C                   ENDIF
085900030715     C                   ENDIF
086000030715     C* Incremento il contatore d campo
086100030715     C                   EVAL      i = i +1
086200030715     C                   ENDDO
086300030715     C*
086400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086500030715     C* delimitazione testo
086600030715     C                   EVAL      i = 1
086700030715     C                   DOW       i < %elem(SkSplitFLD)
086800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087000030715     C                   EVAL      i = i + 1
087100030715     C                   ENDDO
087200030715     C                   EVAL      wGiro = 1
087300030715     C                   ELSE
087400030715     C                   EVAL      wGiro = 2
087500031204     C**********
087600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087700031204     C* d separatore campo
087800031204     C**********
087900031204     C                   EVAL      vindta = %trim(vindta)
088000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088100031204     C                   EVAL      vindta = %subst(vindta:2)
088200031204     C                   ENDDO
088300031204     C*
088400031204     C                   Z-ADD     *zeros        lunghInput        4 0
088500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088900110912     C                   ENDDO
089000030715     C**********
089100030715     C* Effettuo lo split del campo dati d input
089200030715     C**********
089300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089400030715     C                   DOW       posDa <= %len(%trim(vindta))
089500030715     C*
089600030715     C* Gestisco il 1° campo
089700030715     C                   IF        i = 1
089800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089900030715     C                   EVAL      posDa = 1
090000030715     C* Eseguo lo scan x trovare la fine del primo campo
090100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C* X i campi successivi al 1°
090600030715     C                   ELSE
090700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090800030715     C                   EVAL      posDa = posA + 1
090900031204     C* Verifico che nn vi sia il campo nn valorizzato
091000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091100031204     C* Se campo nn valorizzato skippo al prossimo
091200031204     C                   EVAL      SkSplitCSV(i) = *blanks
091300031204     C                   EVAL      posA  = posA + 1
091400031204     C                   ELSE
091500030715     C* Eseguo lo scan x trovare la fine del campo corrente
091600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091700030715     C* Gestisco l'ultimo campo
091800030715     C                   IF        posA = *zeros
091900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092000030715     C* A questo punto "estraggo" il campo corrente
092100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092200030715     C                                                    (posA-posDa))
092300030715     C                   LEAVE
092400030715     C                   ELSE
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C                   ENDIF
092900031204     C                   ENDIF
093000030715     C                   ENDIF
093100030715     C* Incremento il contatore d campo
093200030715     C                   EVAL      i = i +1
093300030715     C                   ENDDO
093400030715     C*
093500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093600030715     C* delimitazione testo
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitCSV)
093900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094100030715     C                   EVAL      i = i + 1
094200030715     C                   ENDDO
094300100720     C*
094400100720     C* Stacco un progressivo x numero spedizione
094500130528     C* NO, il numero spedizione viene passato
094600130528 xxx C***                EXSR      repNSP
094700100326     C*
094800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094900030715     C                   EVAL      i = 1
095000030715     C                   DOW       i < %elem(SkSplitFLD)
095100030715     C*
095200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095400030715     C                             SkSplitCSV(i) <> *blanks
095500030721     C*** ATB
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095700030721     C                   EVAL      VABATB = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030715     C*** RSD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096400030715     C* ==
096500130528     C                   ENDIF
096600030721     C*** RD2
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030715     C*** IND
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097200030715     C                   EVAL      VABIND = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** LOD
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** PRD
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030721     C*** NZD
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030715     C*** RMA
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** NOT
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** NT2
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** VCA
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** VAS
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** FFD
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030721     C*** GC1
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** GC2
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TSP
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102100030721     C                   ENDIF
102200040510     C*** CBO
102300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102500040510     C                   ENDIF
102600030721     C*** NAS
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** TIC
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** GCA
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** XCO
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** CTM
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** TCR
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** CTS
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** FTM
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** VAD
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GMA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** GGA
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** GVA
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** TC1
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** TC2
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** SCL
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** RMO
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** NMO
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*
109500030721     C* Reperisco quindi i campi numerici...
109600040510     C*** NSP
109700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109800040510     C                   EVAL      PiStr=SkSplitCSV(i)
109900040510     C                   EXSR      CHKNUM
110000040510     C                   IF        PiInt=*on
110100040510     C                   Z-ADD     PiVal         VABNSP
110200040510     C                   ELSE
110300040510     C                   SETON                                        31
110400040510     C                   EVAL      VABNSP = *zeros
110500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110600040510     C                             + ' ' + 'VABNSP'
110700040510     C                   ENDIF
110800040510     C                   ENDIF
110900071106     C*** RMN
111000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111100071106     C                   EVAL      PiStr=SkSplitCSV(i)
111200071106     C                   EXSR      CHKNUM
111300071106     C                   IF        PiInt=*on
111400071106     C                   Z-ADD     PiVal         VABRMN
111500071106     C                   ELSE
111600071106     C                   SETON                                        32
111700071106     C                   EVAL      VABRMN = *zeros
111800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111900071106     C                             + ' ' + 'VABRMN'
112000071106     C                   ENDIF
112100071106     C                   ENDIF
112200030721     C*** CAD
112300130605     C*** siccome è un traffico estero, il CAP lo gestisco testuale
112400130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112500130605     C                   EVAL      VABCAD = SkSplitCSV(i)
112600130605     C***                EVAL      PiStr=SkSplitCSV(i)
112700130605     C***                EXSR      CHKNUM
112800130605     C***                IF        PiInt=*on
112900130605     C***                Z-ADD     PiVal         Num5_0
113000130605     C***                MOVEL(p)  Num5_0        VABCAD
113100130605     C***                ELSE
113200130605     C***                SETON                                        32
113300130605     C***                EVAL      VABCAD = *zeros
113400130605     C***                EVAL      vinmsg = %trimr(vinmsg)
113500130605     C***                          + ' ' + 'VABCAD'
113600130605     C***                ENDIF
113700130605     C                   ENDIF
113800030721     C*** NCL
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiInt=*on
114300030721     C                   Z-ADD     PiVal         VABNCL
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      VABNCL = *zeros
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800040122     C                             + ' ' + 'VABNCL'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** PKB
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiNum=*on
115600030721     C                   Z-ADD     PiVal         VABPKB
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABPKB = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABPKB'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** CAS
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116600110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116700111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
116800111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
116900120227 xxx C                   EVAL      VABvca = 'EUR'
117000101018     C                   EVAL      wFlgCAS = '1'
117100101018     C                   ENDIF
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiNum=*on
117500030721     C                   Z-ADD     PiVal         VABCAS
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABCAS = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABCAS'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** CCM
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiInt=*on      AND
118800030721     C                             PiVal<=9999999 AND
118900030721     C                             PiVal>*zeros
119000030721     C                   Z-ADD     PiVal         VABCCM
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      VABCCM = *zeros
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500040510     C                             + ' ' + 'VABCCM'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** LNP
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on    AND
120300030721     C                             PiVal<=999   AND
120400030721     C                             PiVal>*zeros
120500030721     C                   Z-ADD     PiVal         VABLNP
120600030721     C                   ELSE
120700040715     C                   SETON                                        32
120800030721     C                   EVAL      VABLNP = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000040510     C                             + ' ' + 'VABLNP'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** NRS
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on AND
121800030721     C                             PiVal<=99
121900030721     C                   Z-ADD     PiVal         VABNRS
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABNRS'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** LNA
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on    AND
123100030721     C                             PiVal<=999   AND
123200030721     C                             PiVal>*zeros
123300030721     C                   Z-ADD     PiVal         VABLNA
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      VABLNA = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABLNA'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** CTR
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on  AND
124600030721     C                             PiVal<=999
124700030721     C                   Z-ADD     PiVal         VABCTR
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABCTR'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** IAS
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiNum=*on
125900030721     C                   Z-ADD     PiVal         VABIAS
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABIAS'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** VLB
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiNum=*on
127100030721     C                   Z-ADD     PiVal         VABVLB
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABVLB'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** QFT
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiNum=*on
128300030721     C                   Z-ADD     PiVal         VABQFT
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABQFT'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** NCD
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiInt=*on      AND
129500030721     C                             PiVal<=9999999
129600030721     C                   Z-ADD     PiVal         VABNCD
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABNCD'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** NCA
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiInt=*on      AND
130800030721     C                             PiVal<=9999999
130900030721     C                   Z-ADD     PiVal         VABNCA
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABNCA'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** ZNC
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on AND
132100030721     C                             PiVal<=99
132200030721     C                   Z-ADD     PiVal         VABZNC
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABZNC'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** DCR
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiInt=*on       AND
133400030721     C                             PiVal<=99999999
133500030721     C                   Z-ADD     PiVal         VABDCR
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABDCR'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** HCR
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiInt=*on   AND
134700030721     C                             PiVal<=9999
134800030721     C                   Z-ADD     PiVal         VABHCR
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABHCR'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** VMD
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiNum=*on
136000030721     C                   Z-ADD     PiVal         VABVMD
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABVMD'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** ANT
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiInt=*on        AND
137200030721     C                             PiVal<=999999999
137300030721     C                   Z-ADD     PiVal         VABANT
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABANT'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000030721     C*** CMO
138100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiInt=*on
138500030721     C                   Z-ADD     PiVal         Num5_0
138600030721     C                   MOVEL(p)  Num5_0        VABCMO
138700030721     C                   ELSE
138800030721     C                   SETON                                        32
138900030721     C                   EVAL      VABCMO = *zeros
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABCMO'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400040510     C*
139500040510     C* Reperisco i campi del FIVAT
139600040510     C*** VATNOT
139700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139900040510     C                   ENDIF
140000040510     C*** VATTRC
140100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140300040510     C                   ENDIF
140400060331     C*** VATNOT_A
140500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140700060331     C                   ENDIF
140800060331     C*** VATNOT_B
140900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141100060331     C                   ENDIF
141200060331     C*** VATNOT_E
141300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141500060331     C                   ENDIF
141600110927     C*** VATNOT_I
141700110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141800110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
141900110927     C                   ENDIF
142000070823     C*
142100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142200120301
142300140310      *** W_VABNSP: è VABNSP/VABRMN/VABRMA, i numerici solo gli ultimi char a dx numerici
142400140310      *    mentre RMA è la composizione dei primi 6 char + dall'11° in poi
142500130528     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
142600130625     C                   EVAL      w50 = %trim(SkSplitCSV(i))
142700130625     C* trovo l'ultimo carattere non numerico e non blank
142800130625     C                   EVAL      wX = %checkR(num : W50) + 1
142900130625
143000140310     C                   EVAL      VABNSP = %dec(
143100130625     C                                   %subst(w50:wX:
143200130625     C                                    %scan(' ':w50)-wX)
143300140310     C                                       :7:0)
143400140310     C                   EVAL      VABRMN = VABNSP
143500140310     C                   EVAL      VABRMA = %subst(w50:1:6) +
143600140310     C                                      %subst(w50:11:9)
143700120411     C                   ENDIF
143800070925     C*
143900071029     C***  <===  -----------------------------------------
144000040510     C*
144100030715     C                   ENDIF
144200030715     C                   EVAL      i = i + 1
144300030715     C                   ENDDO
144400070925     C*
144500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144600120411     C*
144700130626     C* Se la nazione che mi è stata passata è I o IT la pulisco, altrimenti non calcola la prov.
144800130626     C                   IF        VABNZD = 'I' or
144900130626     C                             VABNZD = 'IT'
145000130626     C                   EVAL      VABNZD = *blank
145100130626     C                   ENDIF
145200100831     C*
145300070925     C*
145400071029     C***  <===  -----------------------------------------
145500070925     C*
145600070925     C* Considerazioni finali su CBO/CAS
145700100318     C                   IF        wFlgCAS = '1'    OR
145800100318     C                             VABCAS  > *zeros
145900130510     C                   EVAL      VABTIC = 'TM'
146000070925     C                   IF        VABCBO = '1'
146100070925     C                   EVAL      VABCBO = '4'
146200070925     C                   ENDIF
146300070925     C                   IF        VABCBO = '2'
146400070925     C                   EVAL      VABCBO = '6'
146500070925     C                   ENDIF
146600070925     C                   ENDIF
146700070925     C***  <===  ----------------------------
146800040510     C*
146900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147000040510     C                   IF        VABNSP = *zeros
147100040510     C                   SETON                                        31
147200040510     C                   ENDIF
147300040715     C*
147400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147500040715     C                   IF        VABLNP = *zeros
147600040715     C                   SETON                                        31
147700040715     C                   ENDIF
147800040510     C*
147900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148000120411     C                   IF        VABRMN = *zeros
148100120411     C                   Z-ADD     VABNSP        VABRMN
148200120411     C                   ENDIF
148300120328     C* IN QUESTO CASO LO FORZO = 1
148400120411     C***                IF        VABRMN = *zeros
148500120411     C***                Z-ADD     1             VABRMN
148600120411     C***                ENDIF
148700040510     C*
148800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148900040510     C                   IF        VABRMA = *blanks
149000040714     C                   MOVEL     VABRMN        VABRMA
149100040510     C                   ENDIF
149200040128     C*
149300040510     C* Se provincia nn valorizzata la reperisco
149400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149500110912     C                   IF        VABPRD =  *blanks AND
149600110912     C                             VABNZD =  *blanks AND
149700110912     C                             VABCAD <> *blanks AND
149800110912     C                             VABLOD <> *blanks
149900040128     C                   CLEAR                   TISI95DS
150000040128     C                   EVAL      I95TCN = '3'
150100040128     C                   Z-ADD     datcor        I95DAT
150200040128     C                   EVAL      I95NAR = VABNZD
150300040128     C                   EVAL      I95CAP = VABCAD
150400040128     C                   EVAL      I95LOC = VABLOD
150500040128     C                   CALL      'TISI95R'
150600040128     C                   PARM                    TISI95DS
150700040128     C                   EVAL      VABPRD = O95PRV
150800040510     C                   ENDIF
150900020204     C*
151000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151100020204     C                   EXSR      CHKIMPDIV
151200030715     C                   ENDIF
151300020204     C*
151400010607     C                   ENDSR
151500010607     C*----------------------------------------------------*
151600010601
151700020204
151800020204     C*----------------------------------------------------*
151900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152000020204     C*----------------------------------------------------*
152100020204     C     CHKIMPDIV     BEGSR
152200020204     C*
152300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152400020204     C                   Z-ADD     *zeros        wrkDec            9 9
152500020204     C*
152600020204     C* Come prima cosa effettuo considerazioni sulla divisa
152700020204     C                   IF        vabIAS > *zeros
152800020204     C                   IF        vabVAS <> 'EUR'
152900020204     C                   EVAL      vabVAS =  'ITL'
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C                   IF        vabCAS > *zeros
153400020204     C                   IF        vabVCA <> 'EUR'
153500020204     C                   EVAL      vabVCA =  'ITL'
153600020204     C                   ENDIF
153700020204     C                   ENDIF
153800020204     C*
153900020204     C                   IF        vabVMD > *zeros
154000020204     C                   IF        vabVAD <> 'EUR'
154100020204     C                   EVAL      vabVAD =  'ITL'
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154600020204     C                   Z-ADD     vabIAS        wrkDec
154700020204     C                   IF        wrkDec > *zeros
154800020204     C                   IF        vabVAS = 'ITL'
154900020204     C                   EVAL      vabIAS = *zeros
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155400020204     C                   Z-ADD     vabCAS        wrkDec
155500020204     C                   IF        wrkDec > *zeros
155600020204     C                   IF        vabVCA = 'ITL'
155700020204     C                   EVAL      vabCAS = *zeros
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156200020204     C                   Z-ADD     vabVMD        wrkDec
156300020204     C                   IF        wrkDec > *zeros
156400020204     C                   IF        vabVAD = 'ITL'
156500020204     C                   EVAL      vabVMD = *zeros
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C                   ENDSR
157000020204     C***
157100010330
157200100318
157300100318
157400100318     C*----------------------------------------------------*
157500100318     C*  CONTROLLO NUMERICITA' CAMPI
157600100318     C*----------------------------------------------------*
157700100318     C     CHKNUM        BEGSR
157800100318     C*
157900100318     C                   IF        PiDecChr = *blanks
158000100318     C                   EVAL      PiDecChr = CharNUM
158100100318     C                   ENDIF
158200100318     C*
158300100318     C                   callp     UBISNUM_Check(PiStr
158400100318     C                                          :PiDecChr
158500100318     C                                          :PiVal
158600100318     C                                          :PiNum
158700100318     C                                          :PiInt)
158800100318     C*
158900100318     C                   ENDSR
159000100318     C***
159100010601
159200010601
159300010601
159400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159500010601     C     repfil        BEGSR
159600010601     C*
159700010601     C                   if        invfil = *zeros and
159800010601     C                             depfil > *zeros and
159900010629     C                             (vinflg = *blanks or
160000010629     C                              vinflg = *zeros)
160100010601     C
160200010601     C                   eval      invfil = depfil
160300010601     C                   endif
160400010601     C*
160500010601     C                   if        depfil <> invfil and
160600010601     C                             invfil > *zeros
160700010601     C                   eval      flgMulti = '1'
160800010601     C                   if        vinflg = *blanks
160900010601     C                   add       1             cntNonEl
161000010601     C                   endif
161100010601     C                   endif
161200010601     C*
161300010601     C                   if        vinflg = '2'
161400010601     C                   eval      flgStato = '2'
161500010601     C                   endif
161600010601     C*
161700010601     C                   ENDSR
161800010601     C***
161900010601
162000010601
162100010601
162200010330
162300040119      /TITLE Invio dei dati al punto operativo.
162400040119     C     invio         BEGSR
162500040119     C*
162600040119     C* 1° invio FIVAT
162700040119     C                   reset                   dscmz
162800040119     C                   move      vlrpoi        cmzdst
162900040119     C                   eval      cmzfld = 'FIVATWWR'
163000040119     C                   eval      cmzmbd = vlrhdl
163100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163200040119     C***                if        prmfir = *blanks
163300040119     C                   eval      cmzfla = 'FIVAT00F'
163400040119     C                   eval      cmzmba = 'FIVAT00F'
163500040119     C***                else
163600040119     C***                eval      cmzfla = prmfir
163700040119     C***                eval      cmzmba = prmfir
163800040119     C***                endif
163900040119     C                   eval      cmznrr = *zeros
164000040119     C                   move      §ctrokvt      cmznrr
164100040119     C                   eval      cmzlba = vlrfl1
164200040119     C                   call(e)   'TIS711C'
164300040119     C                   parm                    dscmz
164400040119     C                   parm      *blanks       esito
164500040119     C                   if        %error
164600040119     C                             or cmzerr = '1'
164700040119     C                             or esito  = '1'
164800040119     C                   eval      wrkesito = '3'
164900040119     C                   else
165000040119     C*
165100040119     C* 2° invio FIVAB
165200040119     C                   reset                   dscmz
165300040119     C                   move      vlrpoi        cmzdst
165400040119     C                   eval      cmzfld = vlrfou
165500040119     C                   eval      cmzmbd = vlrhdl
165600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165700040119     C***                if        prmfir = *blanks
165800040119     C                   eval      cmzfla = 'FIVAB00F'
165900040119     C                   eval      cmzmba = 'FIVAB00F'
166000040119     C***                else
166100040119     C***                eval      cmzfla = prmfir
166200040119     C***                eval      cmzmba = prmfir
166300040119     C***                endif
166400040119     C                   eval      cmznrr = *zeros
166500040119     C                   move      §ctrokvb      cmznrr
166600040119     C                   eval      cmzlba = vlrfl1
166700040119     C                   call(e)   'TIS711C'
166800040119     C                   parm                    dscmz
166900040119     C                   parm      *blanks       esito
167000040119     C                   if        %error
167100040119     C                             or cmzerr = '1'
167200040119     C                             or esito  = '1'
167300040119     C                   eval      wrkesito = '3'
167400040119     C                   endif
167500040119     C                   endif
167600040119     C*
167700040119     C                   ENDSR
167800040119     C***
167900010601
168000010601
168100010601
168200010601
168300010601      /TITLE Invio dei dati al punto operativo.
168400010601     C     opeini        BEGSR
168500010601     C*
168600010601     C* Inizializzo flag e contatori operativi
168700010601     C                   movel     '0'           flgGiro           1
168800010601     C                   movel     '0'           flgMulti          1
168900010601     C                   movel     '1'           flgStato          1
169000010615     C                   movel     '0'           flgOk             1
169100010601     C                   z-add     *zeros        cntNonEl         10 0
169200010601     C                   z-add     *zeros        depfil            3 0
169300010601     C                   z-add     *zeros        invfil            3 0
169400010601     C*
169500010601     C                   ENDSR
169600010601     C***
169700070326
169800070326
169900070326
170000070326
170100070326     C     *pssr         BEGSR
170200070329     C*
170300070329     C                   if        %open(tivin00r)
170400070329     C                   close     tivin00r
170500070329     C                   endif
170600070329     C                   if        %open(fivabwwr)
170700070329     C                   close     fivabwwr
170800070329     C                   endif
170900070329     C                   if        %open(fivatwwr)
171000070329     C                   close     fivatwwr
171100070329     C                   endif
171200070326     C*
171300070326     C* Effettuo la chiamata al CLLE preposto
171400070326     C                   call(e)   'TITVVTC'
171500070326     C                   parm                    parccm
171600070326     C                   parm                    parmbr
171700070326     C                   parm      '2'           paropz
171800070326     C*
171900070326     C                   eval      wrkesito = '2'
172000070404     C*
172100070404     C                   seton                                        LR
172200070326     C*
172300070326     C                   ENDSR     '*CANCL'
172400070326     C***
172500100318
172600100318
172700100318
172800100318     C     normCSV       BEGSR
172900100318     C*
173000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173100100318     C* separatori campi
173200100318     C                   if        CharTXT <> *blanks
173300100318     C                   z-add     *zeros        wLenVINDTA        4 0
173400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173500100318     C                   setoff                                       80
173600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173900100318     C                   if        not *in80
174000100318     C                   seton                                        80
174100100318     C                   else
174200100318     C                   setoff                                       80
174300100318     C                   endif
174400100318     C                   endif
174500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174600100318     C                             and *in80 = *on
174700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174800100318     C                   endif
174900100318     C                   enddo
175000100318     C                   endif
175100100318     C*
175200100318     C                   ENDSR
175300100318     C***
175400070326
175500100326
175600100326
175700100326     C*----------------------------------------------------*
175800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175900100326     C*----------------------------------------------------*
176000100326     C     REPNSP        BEGSR
176100100326     C*
176200100326     C* NSP => Stacco un numeratore da AZNUM
176300100326     C                   clear                   TRUL33DS
176400100326     C                   eval      I33OPE = *zeros
176500100326     C                   eval      I33CNU = 302
176600100326     C                   eval      I33NUM = 1
176700100326     C                   movel     TRUL33DS      KPJBU
176800100326     C                   call      'TRUL33R'
176900100326     C                   parm                    KPJBA
177000100326     C                   movel     KPJBU         TRUL33DS
177100100326     C                   if        O33ERR = *zeros
177200100326     C                   z-add     O33NRF        VABNSP
177300100326     C                   z-add     O33NRF        VATNSP
177400100326     C                   else
177500100326     C                   SETON                                        31
177600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177700100326     C                             + ' ' + 'VABNSP VATNSP'
177800100326     C                   endif
177900100326     C*
178000100326     C                   ENDSR
178100010330
178200100326
178300010330
178400000613     C     *inzsr        BEGSR
178500990910     C*
178600990910     C     *entry        plist
178700990920     C                   parm                    tivlrds
178800990921     C                   parm      wrkesito      esito
178900000724     C                   parm                    prmlit
179000000710     C                   parm                    prmfir
179100010330     C*
179200010330     C* CALCOLA LA DATA CORRENTE
179300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179400100318     C                   eval      datcor = %dec(%date() : *ISO)
179500000613     C*
179600000613     C                   ENDSR
179700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179800140310X;X;X;X;X;X;X;X;X;X;X;VABRSD;X;X;X;X;X;VABLOD;X;VABIND;VABNZD;X;VABCAD;X;X;X;
179900130528X;X;W_VABNSP;X;X;
180000130528
180100111116
180200100318
