000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400130528      * il loro nr.DDT è sia VABRMA che VABNSP ma solo gli ultimi 5 char a dx (parte prg)
000500130528      * il peso sarà sempre 0
000600130626      * Se la nazione che mi è stata passata è I o IT la pulisco, altrimenti non calcola la prov.
000700130528      *
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
004200130528     D Intestazioni    s               n   inz(*off)
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
007100130625     D w50             s             50
007200130625     D wX              s              3  0
007300130625     D num             c                   const('1234567890 ')
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
009200100318
009300010330
009400010330
009500010330
009600000913     C                   reset                   rrnum
009700990921     C                   reset                   esito
009800990921     C                   reset                   wrkesito
009900010601     C*
010000010601     C                   exsr      opeini
010100010605     C                   exsr      rwvab
010200010601     C*
010300010601     C                   seton                                        lr
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010900010601     C*--------------------------------------------------------
011000010601     C     PREELA        BEGSR
011100010601     C*
011200010601     C* SE OCCORRE SPEDIRE IN FILIALE
011300010601     C                   if        invfil <> *zeros and
011400010601     C                             flgGiro = '0'
011500010601     C*
011600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010601     C                   eval      flgGiro = '1'
011800010601     C*
011900010601     C                   endif
012000010601     C*
012100010601     C                   ENDSR
012200010601     C***
012300010601
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010601     C*--------------------------------------------------------
013000010601     C     ENDELA        BEGSR
013100040119     C*
013200040119     C* Effettuo la chiamata al CLLE preposto
013300040119     C                   call(e)   'TITVVTC'
013400040510     C                   parm                    parccm
013500040119     C                   parm                    parmbr
013600040119     C                   parm      '2'           paropz
013700050201     C*
013800050201     C* Effettuo lancio TISI95 solo x chiusura
013900050201     C                   CLEAR                   TISI95DS
014000050201     C                   EVAL      I95TLA = 'C'
014100050201     C                   CALL      'TISI95R'
014200050201     C                   PARM                    TISI95DS
014300000616     C*
014400010601     C                   ENDSR
014500010601
014600010601
014700010330     C*--------------------------------------------------------
014800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014900010330     C*--------------------------------------------------------
015000010605     C     RWVAB         BEGSR
015100010330     C*
015200010330     C                   if        not %open(tivin00r)
015300010330     C                   open      tivin00r
015400010330     C                   endif
015500021025     C                   if        not %open(fivabwwr)
015600021025     C                   open      fivabwwr
015700010330     C                   endif
015800040119     C*
015900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016000040119     C                   exsr      prevat
016100040119     C*
016200040119     C                   if        chkcall = '0'
016300040119     C*
016400040119     C                   if        not %open(fivatwwr)
016500040119     C                   open      fivatwwr
016600040119     C                   endif
016700010330     C*
016800040119     C                   clear                   §CTROKVB          7 0
016900040119     C                   clear                   §CTROKVT          7 0
017000040119     C                   clear                   §CTRMO            7 0
017100040119     C                   clear                   §CTRNO            7 0
017200120410     C*
017300120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017400120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017500120410     C                   IF        Intestazioni = *off
017600120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017700120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017800120410     C                   exsr      inzvar
017900120410     C                   exsr      impvab
018000120410     C                   ENDIF
018100010330     C*
018200030822     C                   do        *HIVAL
018300010330     C*
018400030822     C                   read      tivin00r                               70
018500010618     C*
018600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018700010618     C                   if        vinflg = '1'
018800010618     C                   eval      flgOk = '1'
018900010618     C                   endif
019000040510     C*
019100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019200040510     C                   z-add     *zeros        wLenVINDTA        4 0
019300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019700040510     C                   z-add     1             wFlgVINDTA
019800040510     C                   leave
019900040510     C                   endif
020000040510     C                   enddo
020100010618     C*
020200040510     C                   if        vindta > *blanks AND
020300040510     C                             wFlgVINDTA = 1
020400010330     C                   add       1             rrnum
020500010330     C*
020600010601     C                   if        *in70 = *off and
020700010330     C                             (vinflg = *blanks
020800010330     C                              or vinflg = '0'
020900010330     C                              or vinflg = '2')
021000010330     C*
021100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021200010711     C                   if        vinflg = *blanks or vinflg = '0'
021300010711     C                   clear                   vinmsg
021400010711     C                   endif
021500010601     C*
021600070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021700130605     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021800130605     C* evitare che vengano elisi i blank
021900130605     C                   eval      vindta  = '*' + %trim(vindta)
022000070925     C*
022100080527     C                   exsr      inzvar
022200100318     C                   exsr      normCSV
022300010605     C                   exsr      impvab
022400010601     C*
022500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022600010605     C                   eval      depfil = VABLNP
022700010601     C                   exsr      repfil
022800010601     C                   if        depfil = invfil
022900021025     C                   if        vlrpoi = 999
023000030822     C                   move(P)   invfil        VABFGS
023100021025     C                   else
023200030822     C                   move(P)   vlrpoi        VABFGS
023300021025     C                   endif
023400010601     C*
023500010601     C                   exsr      PREELA
023600010601     C*
023700010604     C* Ebbene...
023800010604     C*
023900040119     C  N31              add       1             §CTROKVB
024000040119     C   32              add       1             §CTRMO
024100040119     C   31              add       1             §CTRNO
024200080418     C*
024300030822     C                   if        wGiro = 2
024400080513     C* Scrivo sempre il VAT
024500080513     C  N31              exsr      wrivat
024600110201     C                   exsr      chkvab
024700080513     C                   endif
024800010604     C*
024900010604     C                   if        *in31 = *off and
025000010604     C                             *in32 = *off
025100010604     C                   eval      vinflg = '1'
025200010604     C                   else
025300010604     C                   eval      vinflg = '2'
025400010604     C                   endif
025500010604     C                   endif
025600010601     C*
025700010604     C                   endif
025800010604     C*
025900010330     C                   else
026000010330     C                   eval      vinflg = '1'
026100010330     C                   endif
026200010601     C*
026300010601     C  N70              update    tivin000
026400010330     C*
026500030822     C  N70              enddo
026600040119     C*
026700080527     C* Verifico dati rimasti "in cannna"...
026800080527     C                   seton                                        65
026900110201     C                   exsr      chkvab
027000080527     C*
027100040119     C                   endif                                                  (endif - chkcall)
027200010601     C*
027300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027400010601     C                   if        cntNonEl = *zeros or
027500010601     C                             flgMulti = '0'
027600010330     C* Se non ci sono record con errori ...
027700010601     C                   if        §ctrno = 0 and
027800010604     C                             §ctrmo = 0 and
027900010601     C                             flgStato <> '2'
028000010330     C* ... restituisco esito OK.
028100010330     C                   eval      wrkesito = '0'
028200010330     C                   else
028300040119     C                   if        §ctrokvb > 0
028400010330     C                   eval      wrkesito = '1'
028500010330     C                   else
028600010615     C                   if        flgOk = '0'
028700010615     C                   eval      wrkesito = '2'
028800010615     C                   else
028900010615     C                   eval      wrkesito = '6'
029000010615     C                   endif
029100010330     C                   endif
029200010330     C                   endif
029300010601     C                   else
029400010601     C                   eval      wrkesito = '9'
029500010601     C                   endif
029600010330     C*
029700010330     C                   if        %open(tivin00r)
029800010330     C                   close     tivin00r
029900010330     C                   endif
030000021025     C                   if        %open(fivabwwr)
030100021025     C                   close     fivabwwr
030200010330     C                   endif
030300040512     C                   if        %open(fivatwwr)
030400040512     C                   close     fivatwwr
030500040512     C                   endif
030600010601     C*
030700010601     C                   if        vlrpoi <> 999
030800010601     C                   eval      invfil = vlrpoi
030900010601     C                   endif
031000010330     C*
031100040119     C                   if        §ctrokvb > 0
031200010601     C                             and invfil > *zeros
031300010330     C                   exsr      invio
031400010330     C                   endif
031500010601     C*
031600010618     C                   if        flgGiro = '1'
031700010601     C                   exsr      endela
031800010618     C                   endif
031900010330     C*
032000010330     C                   ENDSR
032100010330     C***
032200080527
032300080527
032400080527
032500080527     C*----------------------------------------------------*
032600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032700080527     C*----------------------------------------------------*
032800100413     C     CHKVAB        BEGSR
032900110201     C*
033000110201     C* Se richiesti ignorare colli / peso / volume
033100110315 xxx C***                if        *in35
033200110315 xxx C***                z-add     *zeros        VABNCL
033300110315 xxx C***                z-add     *zeros        VABPKB
033400110315 xxx C***                z-add     *zeros        VABVLB
033500110315 xxx C***                endif
033600080527     C*
033700080527     C* Scrivo il VAB solo a rottura d spedizione
033800080527     C                   if        depspe = *zeros
033900080527     C  N31              eval      fivabds_sav = fivabds
034000080527     C                   eval      depspe = vabNSP
034100080527     C                   else
034200080527     C                   if        depspe = vabNSP AND *in65 = *off
034300100413     C   51              add       VABNCL        sav_VABNCL
034400100413     C   51              add       VABPKB        sav_VABPKB
034500100413     C   51              add       VABVLB        sav_VABVLB
034600100720     C   52              z-add     VABNCL        sav_VABNCL
034700100720     C   52              z-add     VABPKB        sav_VABPKB
034800100720     C   52              z-add     VABVLB        sav_VABVLB
034900100720     C   53              add       VABNCL        sav_VABNCL
035000100720     C   53              z-add     VABPKB        sav_VABPKB
035100100720     C   53              z-add     VABVLB        sav_VABVLB
035200080527     C                   else
035300080527     C  N31              eval      fivabds_ok = fivabds_sav
035400080527     C  N31              exsr      wrivab
035500080527     C  N31              eval      fivabds_sav = fivabds
035600080527     C                   eval      depspe = vabNSP
035700080527     C                   endif
035800080527     C                   endif
035900080527     C*
036000080527     C                   ENDSR
036100040119
036200040119
036300040119
036400040119     C*----------------------------------------------------*
036500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036600040119     C*----------------------------------------------------*
036700040510     C     WRIVAB        BEGSR
036800080513     C*
036900080513     C* Gestisco l'eventuale rottura x numero spedizione
037000080527     C                   if        VABNSP <> depspe or *in65 = *on
037100070301     C                   if        wVATNOT_E <> *blanks
037200070301     C                   eval      VABCTM = '7Q'
037300070301     C                   endif
037400100413     C                   write     fivab000
037500080513     C                   endif
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038300040119     C*----------------------------------------------------*
038400040510     C     WRIVAT        BEGSR
038500060512     C*
038600060512     C* Inizializzo i campi chiave
038700060512     C                   eval      VATFGS = VABFGS
038800060512     C                   eval      VATAAS = VABAAS
038900060512     C                   eval      VATCCM = VABCCM
039000070823     C                   eval      VATNRS = VABNRS
039100060512     C                   eval      VATNSP = VABNSP
039200060512     C                   eval      VATLNP = VABLNP
039300040119     C*
039400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039500040510     C                   if        VATTRC = *blanks
039600040510     C                   eval      VATTRC = 'E'
039700040510     C                   endif
039800110318     C                   if        VATNOT <> *blanks
039900040510     C                   write     FIVAT000
040000040119     C                   add       1             §CTROKVT
040100110318     C                   endif
040200060331     C*
040300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040400060331     C                   if        wVATNOT_A <> *blanks
040500060331     C                   eval      VATNOT = wVATNOT_A
040600060331     C                   eval      VATTRC = 'A'
040700060331     C                   write     FIVAT000
040800060331     C                   add       1             §CTROKVT
040900060331     C                   endif
041000060331     C*
041100060331     C                   if        wVATNOT_B <> *blanks
041200060331     C                   eval      VATNOT = wVATNOT_B
041300060331     C                   eval      VATTRC = 'B'
041400060331     C                   write     FIVAT000
041500060331     C                   add       1             §CTROKVT
041600060331     C                   endif
041700060331     C*
041800120320     C*
041900120410     C* i rcd di FIVAT con tipo E sono 1 in quanto è una spedizione monocollo
042000060331     C                   if        wVATNOT_E <> *blanks
042100110201     C                   eval      VATNOT = wVATNOT_E
042200060331     C                   eval      VATTRC = 'E'
042300120411     C*
042400120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
042500120411     C*
042600120411     C     1             DO        VABNCL        wNumCollo         3 0
042700120411     C*** da analisi del POC il progressivo colli è max 3 char e deve ccontiene gli zeri non
042800120411     C*** significativi
042900120411     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
043000120411     C                                %editc(wNumCollo:'X')
043100120410     C                   write     FIVAT000
043200120410     C                   add       1             §CTROKVT
043300120411     C                   ENDDO
043400120320     C*
043500120320     C                   endif
043600120320     C*
043700110927     C*
043800110927     C                   if        wVATNOT_I <> *blanks
043900110927     C                   eval      VATNOT = wVATNOT_I
044000110927     C                   eval      VATTRC = 'I'
044100110927     C                   write     FIVAT000
044200110927     C                   add       1             §CTROKVT
044300110927     C                   endif
044400040119     C*
044500040119     C                   ENDSR
044600040119
044700040119
044800040119
044900040119     C*----------------------------------------------------*
045000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045100040119     C*----------------------------------------------------*
045200040119     C     PREVAT        BEGSR
045300040119     C*
045400040119     C* Compongo il nome del membro da dare al FIVATWWR
045500040119     C                   eval      parmbr = vlrhdl
045600040119     C                   movel     'M'           parmbr
045700040510     C                   eval      parccm = vlrksc
045800040119     C                   eval      paropz = '1'
045900040119     C* Effettuo la chiamata al CLLE preposto
046000040119     C                   call(e)   'TITVVTC'
046100040119     C                   parm                    parccm
046200040119     C                   parm                    parmbr
046300040119     C                   parm                    paropz
046400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046500040119     C                   if        %error
046600040119     C                   movel     '1'           chkcall
046700040119     C                   else
046800040119     C                   movel     '0'           chkcall
046900040119     C                   endif
047000040119     C*
047100040119     C                   ENDSR
047200010601
047300010601
047400010601
047500010330     C*----------------------------------------------------*
047600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047700010330     C*----------------------------------------------------*
047800040119     C     INZVAR        BEGSR
047900080527     C*
048000080527     C                   setoff                                       65
048100010330     C*
048200030822     C* Inizializzo variabili di wrk
048300020204     C                   Z-ADD     *zeros        Num5_0            5 0
048400101018     C                   MOVEL     *blanks       wFlgCAS
048500060331     C                   MOVEL     *blanks       wVATNOT_A
048600060331     C                   MOVEL     *blanks       wVATNOT_B
048700060331     C                   MOVEL     *blanks       wVATNOT_E
048800110927     C                   MOVEL     *blanks       wVATNOT_I
048900120411     C                   MOVEL     *blanks       wNOT
049000120411     C                   MOVEL     *blanks       Data8A
049100030822     C*
049200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049300030822     C                   CLEAR                   SkSplitCSV
049400030822     C                   EVAL      i = 1
049500030822     C                   EVAL      posDa = *zeros
049600030822     C                   EVAL      posA  = *zeros
049700080527     C*
049800080527     C                   CLEAR                   FIVABDS
049900080527     C                   CLEAR                   FIVAT000
050000030822     C*
050100030822     C* Reimposto i valori di default
050200030822     C                   EXSR      DEFCAM
050300030822     C*
050400010330     C                   ENDSR
050500010330     C*----------------------------------------------------*
050600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050700010330     C*----------------------------------------------------*
050800010330     C     DEFCAM        BEGSR
050900030822     C*
051000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051100030715     C* e delimitatore testo.
051200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051500100318     C*
051600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051700100318     C                   EVAL      CharSOS = CharNUM
051800030822     C*
051900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052000100720     C                   SETOFF                                       505152
052100100720     C                   SETOFF                                       53
052200100413     C                   SELECT
052300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052400100720     C                   SETON                                        50
052500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052600100413     C                   SETON                                        51
052700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052800100720     C                   SETON                                        52
052900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053000100720     C                   SETON                                        53
053100100413     C                   ENDSL
053200030822     C*
053300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053400030822     C                   EVAL      posDaDft = 1
053500030822     C                   EVAL      posADft  = 0
053600030822     C                   EVAL      wGiroDft = 0
053700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053800030822     C                             posDaDft > 0
053900030822     C*
054000030822     C* Gestisco il 1° giro
054100030822     C                   IF        wGiroDft = 0
054200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054400030822     C* Incremento il contatore dei "giri"
054500030822     C                   EVAL      wGiroDft = 1
054600030822     C                   ELSE
054700030822     C                   EVAL      posDaDft = posADft
054800030822     C                   ENDIF
054900030822     C* Eseguo lo scan x trovare la fine del campo corrente
055000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055100030822     C*
055200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055400030822     C                   IF        posDaDft > 0 AND
055500030822     C                             posADft  > 0
055600030822     C* NCL
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'NCL'
056100030822     C                   EVAL      PiStr=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   EXSR      CHKNUM
056500030822     C                   IF        PiInt=*on
056600030822     C                   Z-ADD     PiVal         VABNCL
056700030822     C                   ENDIF
056800030822     C                   ENDIF
056900030822     C* CCM
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'CCM'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900030822     C                   Z-ADD     PiVal         VABCCM
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* LNP
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'LNP'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiInt=*on
059200030822     C                   Z-ADD     PiVal         VABLNP
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* NRS
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'NRS'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500030822     C                   Z-ADD     PiVal         VABNRS
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* CTR
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'CTR'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiInt=*on
061800030822     C                   Z-ADD     PiVal         VABCTR
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* PKB
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'PKB'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiNum=*on
063100030822     C                   Z-ADD     PiVal         VABPKB
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* VLB
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'VLB'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiNum=*on
064400030822     C                   Z-ADD     PiVal         VABVLB
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* QFT
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'QFT'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiNum=*on
065700030822     C                   Z-ADD     PiVal         VABQFT
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* CBO
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'CBO'
066500030822     C                   EVAL      VABCBO=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* TSP
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'TSP'
067400030822     C                   EVAL      VABTSP=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* VAS
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'VAS'
068300030822     C                   EVAL      VABVAS=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* VCA
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'VCA'
069200030822     C                   EVAL      VABVCA=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600070925     C* TIC
069700070925     C                   IF        %subst(
069800070925     C                             %subst(vlrppt:posDaDft+1:
069900070925     C                             posADft-posDaDft-1):1:3)
070000070925     C                             = 'TIC'
070100070925     C                   EVAL      VABTIC=%trim(%subst(
070200070925     C                             %subst(vlrppt:posDaDft+1:
070300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400070925     C                   ENDIF
070500030822     C* GCA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'GCA'
071000030822     C                   EVAL      VABGCA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* CTM
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'CTM'
071900030822     C                   EVAL      VABCTM=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* FFD
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'FFD'
072800030822     C                   EVAL      VABFFD=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* VAD
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'VAD'
073700030822     C                   EVAL      VABVAD=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* GMA
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'GMA'
074600030822     C                   EVAL      VABGMA=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* GGA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GGA'
075500030822     C                   EVAL      VABGGA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* GVA
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'GVA'
076400030822     C                   EVAL      VABGVA=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* TC1
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'TC1'
077300030822     C                   EVAL      VABTC1=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* TC2
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'TC2'
078200030822     C                   EVAL      VABTC2=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600040714     C* VATTRC
078700040714     C                   IF        %subst(
078800040714     C                             %subst(vlrppt:posDaDft+1:
078900040714     C                             posADft-posDaDft-1):1:3)
079000040714     C                             = 'TRC'
079100040714     C                   EVAL      VATTRC=%trim(%subst(
079200040714     C                             %subst(vlrppt:posDaDft+1:
079300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400040714     C                   ENDIF
079500030822     C* ...
079600030822     C                   ENDIF
079700030822     C                   ENDDO
079800020204     C*
079900010330     C                   ENDSR
080000010607     C*----------------------------------------------------*
080100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080200010607     C*----------------------------------------------------*
080300010607     C     IMPVAB        BEGSR
080400010607     C*
080500010607     C                   SETOFF                                       3132
080600110315 xxx C***                SETON                                        35
080700030515     C*
080800010607     C                   MOVEL     datcor        VABAAS
080900010607     C                   MOVE      datcor        VABMGS
081000010607     C*
081100030715     C**********
081200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081300030715     C**********
081400030715     C                   IF        wGiro = *zeros
081500040113     C*
081600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081900080429     C                             %trim(intcol(5))<>*blanks
082000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082200080429     C                                      %trim(intcol(5))
082300070823     C     ';':CharCSV   XLATE     vindta        vindta
082400040510     C                   ENDIF
082500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082600031201     C     minu:maiu     XLATE     vindta        vindta
082700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082800030715     C                   DOW       posDa <= %len(%trim(vindta))
082900030715     C*
083000030715     C* Gestisco il 1° campo
083100030715     C                   IF        i = 1
083200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083300030715     C                   EVAL      posDa = 1
083400030715     C* Eseguo lo scan x trovare la fine del primo campo
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C* X i campi successivi al 1°
084000030715     C                   ELSE
084100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084200030715     C                   EVAL      posDa = posA + 1
084300030715     C* Eseguo lo scan x trovare la fine del campo corrente
084400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084500030715     C* Gestisco l'ultimo campo
084600030715     C                   IF        posA = *zeros
084700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C                   LEAVE
085200030715     C                   ELSE
085300030715     C* A questo punto "estraggo" il campo corrente
085400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085500030715     C                                                    (posA-posDa))
085600030715     C                   ENDIF
085700030715     C                   ENDIF
085800030715     C* Incremento il contatore d campo
085900030715     C                   EVAL      i = i +1
086000030715     C                   ENDDO
086100030715     C*
086200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086300030715     C* delimitazione testo
086400030715     C                   EVAL      i = 1
086500030715     C                   DOW       i < %elem(SkSplitFLD)
086600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086800030715     C                   EVAL      i = i + 1
086900030715     C                   ENDDO
087000030715     C                   EVAL      wGiro = 1
087100030715     C                   ELSE
087200030715     C                   EVAL      wGiro = 2
087300031204     C**********
087400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087500031204     C* d separatore campo
087600031204     C**********
087700031204     C                   EVAL      vindta = %trim(vindta)
087800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087900031204     C                   EVAL      vindta = %subst(vindta:2)
088000031204     C                   ENDDO
088100031204     C*
088200031204     C                   Z-ADD     *zeros        lunghInput        4 0
088300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088700110912     C                   ENDDO
088800030715     C**********
088900030715     C* Effettuo lo split del campo dati d input
089000030715     C**********
089100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089200030715     C                   DOW       posDa <= %len(%trim(vindta))
089300030715     C*
089400030715     C* Gestisco il 1° campo
089500030715     C                   IF        i = 1
089600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089700030715     C                   EVAL      posDa = 1
089800030715     C* Eseguo lo scan x trovare la fine del primo campo
089900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090000030715     C* A questo punto "estraggo" il campo corrente
090100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090200030715     C                                                    (posA-posDa))
090300030715     C* X i campi successivi al 1°
090400030715     C                   ELSE
090500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090600030715     C                   EVAL      posDa = posA + 1
090700031204     C* Verifico che nn vi sia il campo nn valorizzato
090800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090900031204     C* Se campo nn valorizzato skippo al prossimo
091000031204     C                   EVAL      SkSplitCSV(i) = *blanks
091100031204     C                   EVAL      posA  = posA + 1
091200031204     C                   ELSE
091300030715     C* Eseguo lo scan x trovare la fine del campo corrente
091400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091500030715     C* Gestisco l'ultimo campo
091600030715     C                   IF        posA = *zeros
091700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C                   LEAVE
092200030715     C                   ELSE
092300030715     C* A questo punto "estraggo" il campo corrente
092400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092500030715     C                                                    (posA-posDa))
092600030715     C                   ENDIF
092700031204     C                   ENDIF
092800030715     C                   ENDIF
092900030715     C* Incremento il contatore d campo
093000030715     C                   EVAL      i = i +1
093100030715     C                   ENDDO
093200030715     C*
093300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093400030715     C* delimitazione testo
093500030715     C                   EVAL      i = 1
093600030715     C                   DOW       i < %elem(SkSplitCSV)
093700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093900030715     C                   EVAL      i = i + 1
094000030715     C                   ENDDO
094100100720     C*
094200100720     C* Stacco un progressivo x numero spedizione
094300130528     C* NO, il numero spedizione viene passato
094400130528 xxx C***                EXSR      repNSP
094500100326     C*
094600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094700030715     C                   EVAL      i = 1
094800030715     C                   DOW       i < %elem(SkSplitFLD)
094900030715     C*
095000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095200030715     C                             SkSplitCSV(i) <> *blanks
095300030721     C*** ATB
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095500030721     C                   EVAL      VABATB = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030715     C*** RSD
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096200030715     C* ==
096300130528     C                   ENDIF
096400030721     C*** RD2
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030715     C*** IND
096900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097000030715     C                   EVAL      VABIND = SkSplitCSV(i)
097100030715     C                   ENDIF
097200030715     C*** LOD
097300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097500030715     C                   ENDIF
097600030715     C*** PRD
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030721     C*** NZD
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030715     C*** RMA
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** NOT
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** NT2
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** VCA
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** VAS
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** FFD
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030721     C*** GC1
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** GC2
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** TSP
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101900030721     C                   ENDIF
102000040510     C*** CBO
102100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102300040510     C                   ENDIF
102400030721     C*** NAS
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** TIC
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** GCA
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** XCO
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** CTM
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** TCR
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** CTS
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** FTM
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** VAD
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** GMA
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** GGA
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GVA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** TC1
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** TC2
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** SCL
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** RMO
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** NMO
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*
109300030721     C* Reperisco quindi i campi numerici...
109400040510     C*** NSP
109500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109600040510     C                   EVAL      PiStr=SkSplitCSV(i)
109700040510     C                   EXSR      CHKNUM
109800040510     C                   IF        PiInt=*on
109900040510     C                   Z-ADD     PiVal         VABNSP
110000040510     C                   ELSE
110100040510     C                   SETON                                        31
110200040510     C                   EVAL      VABNSP = *zeros
110300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110400040510     C                             + ' ' + 'VABNSP'
110500040510     C                   ENDIF
110600040510     C                   ENDIF
110700071106     C*** RMN
110800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110900071106     C                   EVAL      PiStr=SkSplitCSV(i)
111000071106     C                   EXSR      CHKNUM
111100071106     C                   IF        PiInt=*on
111200071106     C                   Z-ADD     PiVal         VABRMN
111300071106     C                   ELSE
111400071106     C                   SETON                                        32
111500071106     C                   EVAL      VABRMN = *zeros
111600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111700071106     C                             + ' ' + 'VABRMN'
111800071106     C                   ENDIF
111900071106     C                   ENDIF
112000030721     C*** CAD
112100130605     C*** siccome è un traffico estero, il CAP lo gestisco testuale
112200130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112300130605     C                   EVAL      VABCAD = SkSplitCSV(i)
112400130605     C***                EVAL      PiStr=SkSplitCSV(i)
112500130605     C***                EXSR      CHKNUM
112600130605     C***                IF        PiInt=*on
112700130605     C***                Z-ADD     PiVal         Num5_0
112800130605     C***                MOVEL(p)  Num5_0        VABCAD
112900130605     C***                ELSE
113000130605     C***                SETON                                        32
113100130605     C***                EVAL      VABCAD = *zeros
113200130605     C***                EVAL      vinmsg = %trimr(vinmsg)
113300130605     C***                          + ' ' + 'VABCAD'
113400130605     C***                ENDIF
113500130605     C                   ENDIF
113600030721     C*** NCL
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiInt=*on
114100030721     C                   Z-ADD     PiVal         VABNCL
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      VABNCL = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600040122     C                             + ' ' + 'VABNCL'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** PKB
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiNum=*on
115400030721     C                   Z-ADD     PiVal         VABPKB
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      VABPKB = *zeros
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABPKB'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** CAS
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
116600111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
116700120227 xxx C                   EVAL      VABvca = 'EUR'
116800101018     C                   EVAL      wFlgCAS = '1'
116900101018     C                   ENDIF
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiNum=*on
117300030721     C                   Z-ADD     PiVal         VABCAS
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABCAS = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABCAS'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** CCM
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on      AND
118600030721     C                             PiVal<=9999999 AND
118700030721     C                             PiVal>*zeros
118800030721     C                   Z-ADD     PiVal         VABCCM
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      VABCCM = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300040510     C                             + ' ' + 'VABCCM'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** LNP
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on    AND
120100030721     C                             PiVal<=999   AND
120200030721     C                             PiVal>*zeros
120300030721     C                   Z-ADD     PiVal         VABLNP
120400030721     C                   ELSE
120500040715     C                   SETON                                        32
120600030721     C                   EVAL      VABLNP = *zeros
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800040510     C                             + ' ' + 'VABLNP'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** NRS
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on AND
121600030721     C                             PiVal<=99
121700030721     C                   Z-ADD     PiVal         VABNRS
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABNRS'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** LNA
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on    AND
122900030721     C                             PiVal<=999   AND
123000030721     C                             PiVal>*zeros
123100030721     C                   Z-ADD     PiVal         VABLNA
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      VABLNA = *zeros
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABLNA'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** CTR
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on  AND
124400030721     C                             PiVal<=999
124500030721     C                   Z-ADD     PiVal         VABCTR
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABCTR'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** IAS
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABIAS
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABIAS'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** VLB
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABVLB
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABVLB'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** QFT
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiNum=*on
128100030721     C                   Z-ADD     PiVal         VABQFT
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABQFT'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** NCD
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on      AND
129300030721     C                             PiVal<=9999999
129400030721     C                   Z-ADD     PiVal         VABNCD
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABNCD'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** NCA
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on      AND
130600030721     C                             PiVal<=9999999
130700030721     C                   Z-ADD     PiVal         VABNCA
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABNCA'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** ZNC
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on AND
131900030721     C                             PiVal<=99
132000030721     C                   Z-ADD     PiVal         VABZNC
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABZNC'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** DCR
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on       AND
133200030721     C                             PiVal<=99999999
133300030721     C                   Z-ADD     PiVal         VABDCR
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABDCR'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** HCR
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on   AND
134500030721     C                             PiVal<=9999
134600030721     C                   Z-ADD     PiVal         VABHCR
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABHCR'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** VMD
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiNum=*on
135800030721     C                   Z-ADD     PiVal         VABVMD
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABVMD'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** ANT
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on        AND
137000030721     C                             PiVal<=999999999
137100030721     C                   Z-ADD     PiVal         VABANT
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABANT'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** CMO
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on
138300030721     C                   Z-ADD     PiVal         Num5_0
138400030721     C                   MOVEL(p)  Num5_0        VABCMO
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      VABCMO = *zeros
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABCMO'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200040510     C*
139300040510     C* Reperisco i campi del FIVAT
139400040510     C*** VATNOT
139500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139700040510     C                   ENDIF
139800040510     C*** VATTRC
139900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140100040510     C                   ENDIF
140200060331     C*** VATNOT_A
140300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140500060331     C                   ENDIF
140600060331     C*** VATNOT_B
140700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140900060331     C                   ENDIF
141000060331     C*** VATNOT_E
141100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141300060331     C                   ENDIF
141400110927     C*** VATNOT_I
141500110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141600110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
141700110927     C                   ENDIF
141800070823     C*
141900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142000120301
142100130625      *** W_VABNSP: è sia VABRMA che VABNSP ma solo gli ultimi char a dx numerici
142200130528     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
142300130625     C                   EVAL      w50 = %trim(SkSplitCSV(i))
142400130625     C* trovo l'ultimo carattere non numerico e non blank
142500130625     C                   EVAL      wX = %checkR(num : W50) + 1
142600130625
142700130528     C                   EVAL      VABRMA =
142800130625     C                                   %subst(w50:wX:
142900130625     C                                    %scan(' ':w50)-wX)
143000130528     C                   EVAL      VABNSP = %dec(VABRMA:7:0)
143100120411     C                   ENDIF
143200070925     C*
143300071029     C***  <===  -----------------------------------------
143400040510     C*
143500030715     C                   ENDIF
143600030715     C                   EVAL      i = i + 1
143700030715     C                   ENDDO
143800070925     C*
143900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144000120411     C*
144100130626     C* Se la nazione che mi è stata passata è I o IT la pulisco, altrimenti non calcola la prov.
144200130626     C                   IF        VABNZD = 'I' or
144300130626     C                             VABNZD = 'IT'
144400130626     C                   EVAL      VABNZD = *blank
144500130626     C                   ENDIF
144600100831     C*
144700070925     C*
144800071029     C***  <===  -----------------------------------------
144900070925     C*
145000070925     C* Considerazioni finali su CBO/CAS
145100100318     C                   IF        wFlgCAS = '1'    OR
145200100318     C                             VABCAS  > *zeros
145300130510     C                   EVAL      VABTIC = 'TM'
145400070925     C                   IF        VABCBO = '1'
145500070925     C                   EVAL      VABCBO = '4'
145600070925     C                   ENDIF
145700070925     C                   IF        VABCBO = '2'
145800070925     C                   EVAL      VABCBO = '6'
145900070925     C                   ENDIF
146000070925     C                   ENDIF
146100070925     C***  <===  ----------------------------
146200040510     C*
146300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146400040510     C                   IF        VABNSP = *zeros
146500040510     C                   SETON                                        31
146600040510     C                   ENDIF
146700040715     C*
146800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146900040715     C                   IF        VABLNP = *zeros
147000040715     C                   SETON                                        31
147100040715     C                   ENDIF
147200040510     C*
147300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147400120411     C                   IF        VABRMN = *zeros
147500120411     C                   Z-ADD     VABNSP        VABRMN
147600120411     C                   ENDIF
147700120328     C* IN QUESTO CASO LO FORZO = 1
147800120411     C***                IF        VABRMN = *zeros
147900120411     C***                Z-ADD     1             VABRMN
148000120411     C***                ENDIF
148100040510     C*
148200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148300040510     C                   IF        VABRMA = *blanks
148400040714     C                   MOVEL     VABRMN        VABRMA
148500040510     C                   ENDIF
148600040128     C*
148700040510     C* Se provincia nn valorizzata la reperisco
148800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148900110912     C                   IF        VABPRD =  *blanks AND
149000110912     C                             VABNZD =  *blanks AND
149100110912     C                             VABCAD <> *blanks AND
149200110912     C                             VABLOD <> *blanks
149300040128     C                   CLEAR                   TISI95DS
149400040128     C                   EVAL      I95TCN = '3'
149500040128     C                   Z-ADD     datcor        I95DAT
149600040128     C                   EVAL      I95NAR = VABNZD
149700040128     C                   EVAL      I95CAP = VABCAD
149800040128     C                   EVAL      I95LOC = VABLOD
149900040128     C                   CALL      'TISI95R'
150000040128     C                   PARM                    TISI95DS
150100040128     C                   EVAL      VABPRD = O95PRV
150200040510     C                   ENDIF
150300020204     C*
150400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
150500020204     C                   EXSR      CHKIMPDIV
150600030715     C                   ENDIF
150700020204     C*
150800010607     C                   ENDSR
150900010607     C*----------------------------------------------------*
151000010601
151100020204
151200020204     C*----------------------------------------------------*
151300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
151400020204     C*----------------------------------------------------*
151500020204     C     CHKIMPDIV     BEGSR
151600020204     C*
151700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
151800020204     C                   Z-ADD     *zeros        wrkDec            9 9
151900020204     C*
152000020204     C* Come prima cosa effettuo considerazioni sulla divisa
152100020204     C                   IF        vabIAS > *zeros
152200020204     C                   IF        vabVAS <> 'EUR'
152300020204     C                   EVAL      vabVAS =  'ITL'
152400020204     C                   ENDIF
152500020204     C                   ENDIF
152600020204     C*
152700020204     C                   IF        vabCAS > *zeros
152800020204     C                   IF        vabVCA <> 'EUR'
152900020204     C                   EVAL      vabVCA =  'ITL'
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C                   IF        vabVMD > *zeros
153400020204     C                   IF        vabVAD <> 'EUR'
153500020204     C                   EVAL      vabVAD =  'ITL'
153600020204     C                   ENDIF
153700020204     C                   ENDIF
153800020204     C*
153900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154000020204     C                   Z-ADD     vabIAS        wrkDec
154100020204     C                   IF        wrkDec > *zeros
154200020204     C                   IF        vabVAS = 'ITL'
154300020204     C                   EVAL      vabIAS = *zeros
154400020204     C                   ENDIF
154500020204     C                   ENDIF
154600020204     C*
154700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
154800020204     C                   Z-ADD     vabCAS        wrkDec
154900020204     C                   IF        wrkDec > *zeros
155000020204     C                   IF        vabVCA = 'ITL'
155100020204     C                   EVAL      vabCAS = *zeros
155200020204     C                   ENDIF
155300020204     C                   ENDIF
155400020204     C*
155500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
155600020204     C                   Z-ADD     vabVMD        wrkDec
155700020204     C                   IF        wrkDec > *zeros
155800020204     C                   IF        vabVAD = 'ITL'
155900020204     C                   EVAL      vabVMD = *zeros
156000020204     C                   ENDIF
156100020204     C                   ENDIF
156200020204     C*
156300020204     C                   ENDSR
156400020204     C***
156500010330
156600100318
156700100318
156800100318     C*----------------------------------------------------*
156900100318     C*  CONTROLLO NUMERICITA' CAMPI
157000100318     C*----------------------------------------------------*
157100100318     C     CHKNUM        BEGSR
157200100318     C*
157300100318     C                   IF        PiDecChr = *blanks
157400100318     C                   EVAL      PiDecChr = CharNUM
157500100318     C                   ENDIF
157600100318     C*
157700100318     C                   callp     UBISNUM_Check(PiStr
157800100318     C                                          :PiDecChr
157900100318     C                                          :PiVal
158000100318     C                                          :PiNum
158100100318     C                                          :PiInt)
158200100318     C*
158300100318     C                   ENDSR
158400100318     C***
158500010601
158600010601
158700010601
158800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158900010601     C     repfil        BEGSR
159000010601     C*
159100010601     C                   if        invfil = *zeros and
159200010601     C                             depfil > *zeros and
159300010629     C                             (vinflg = *blanks or
159400010629     C                              vinflg = *zeros)
159500010601     C
159600010601     C                   eval      invfil = depfil
159700010601     C                   endif
159800010601     C*
159900010601     C                   if        depfil <> invfil and
160000010601     C                             invfil > *zeros
160100010601     C                   eval      flgMulti = '1'
160200010601     C                   if        vinflg = *blanks
160300010601     C                   add       1             cntNonEl
160400010601     C                   endif
160500010601     C                   endif
160600010601     C*
160700010601     C                   if        vinflg = '2'
160800010601     C                   eval      flgStato = '2'
160900010601     C                   endif
161000010601     C*
161100010601     C                   ENDSR
161200010601     C***
161300010601
161400010601
161500010601
161600010330
161700040119      /TITLE Invio dei dati al punto operativo.
161800040119     C     invio         BEGSR
161900040119     C*
162000040119     C* 1° invio FIVAT
162100040119     C                   reset                   dscmz
162200040119     C                   move      vlrpoi        cmzdst
162300040119     C                   eval      cmzfld = 'FIVATWWR'
162400040119     C                   eval      cmzmbd = vlrhdl
162500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162600040119     C***                if        prmfir = *blanks
162700040119     C                   eval      cmzfla = 'FIVAT00F'
162800040119     C                   eval      cmzmba = 'FIVAT00F'
162900040119     C***                else
163000040119     C***                eval      cmzfla = prmfir
163100040119     C***                eval      cmzmba = prmfir
163200040119     C***                endif
163300040119     C                   eval      cmznrr = *zeros
163400040119     C                   move      §ctrokvt      cmznrr
163500040119     C                   eval      cmzlba = vlrfl1
163600040119     C                   call(e)   'TIS711C'
163700040119     C                   parm                    dscmz
163800040119     C                   parm      *blanks       esito
163900040119     C                   if        %error
164000040119     C                             or cmzerr = '1'
164100040119     C                             or esito  = '1'
164200040119     C                   eval      wrkesito = '3'
164300040119     C                   else
164400040119     C*
164500040119     C* 2° invio FIVAB
164600040119     C                   reset                   dscmz
164700040119     C                   move      vlrpoi        cmzdst
164800040119     C                   eval      cmzfld = vlrfou
164900040119     C                   eval      cmzmbd = vlrhdl
165000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165100040119     C***                if        prmfir = *blanks
165200040119     C                   eval      cmzfla = 'FIVAB00F'
165300040119     C                   eval      cmzmba = 'FIVAB00F'
165400040119     C***                else
165500040119     C***                eval      cmzfla = prmfir
165600040119     C***                eval      cmzmba = prmfir
165700040119     C***                endif
165800040119     C                   eval      cmznrr = *zeros
165900040119     C                   move      §ctrokvb      cmznrr
166000040119     C                   eval      cmzlba = vlrfl1
166100040119     C                   call(e)   'TIS711C'
166200040119     C                   parm                    dscmz
166300040119     C                   parm      *blanks       esito
166400040119     C                   if        %error
166500040119     C                             or cmzerr = '1'
166600040119     C                             or esito  = '1'
166700040119     C                   eval      wrkesito = '3'
166800040119     C                   endif
166900040119     C                   endif
167000040119     C*
167100040119     C                   ENDSR
167200040119     C***
167300010601
167400010601
167500010601
167600010601
167700010601      /TITLE Invio dei dati al punto operativo.
167800010601     C     opeini        BEGSR
167900010601     C*
168000010601     C* Inizializzo flag e contatori operativi
168100010601     C                   movel     '0'           flgGiro           1
168200010601     C                   movel     '0'           flgMulti          1
168300010601     C                   movel     '1'           flgStato          1
168400010615     C                   movel     '0'           flgOk             1
168500010601     C                   z-add     *zeros        cntNonEl         10 0
168600010601     C                   z-add     *zeros        depfil            3 0
168700010601     C                   z-add     *zeros        invfil            3 0
168800010601     C*
168900010601     C                   ENDSR
169000010601     C***
169100070326
169200070326
169300070326
169400070326
169500070326     C     *pssr         BEGSR
169600070329     C*
169700070329     C                   if        %open(tivin00r)
169800070329     C                   close     tivin00r
169900070329     C                   endif
170000070329     C                   if        %open(fivabwwr)
170100070329     C                   close     fivabwwr
170200070329     C                   endif
170300070329     C                   if        %open(fivatwwr)
170400070329     C                   close     fivatwwr
170500070329     C                   endif
170600070326     C*
170700070326     C* Effettuo la chiamata al CLLE preposto
170800070326     C                   call(e)   'TITVVTC'
170900070326     C                   parm                    parccm
171000070326     C                   parm                    parmbr
171100070326     C                   parm      '2'           paropz
171200070326     C*
171300070326     C                   eval      wrkesito = '2'
171400070404     C*
171500070404     C                   seton                                        LR
171600070326     C*
171700070326     C                   ENDSR     '*CANCL'
171800070326     C***
171900100318
172000100318
172100100318
172200100318     C     normCSV       BEGSR
172300100318     C*
172400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
172500100318     C* separatori campi
172600100318     C                   if        CharTXT <> *blanks
172700100318     C                   z-add     *zeros        wLenVINDTA        4 0
172800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
172900100318     C                   setoff                                       80
173000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173300100318     C                   if        not *in80
173400100318     C                   seton                                        80
173500100318     C                   else
173600100318     C                   setoff                                       80
173700100318     C                   endif
173800100318     C                   endif
173900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174000100318     C                             and *in80 = *on
174100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174200100318     C                   endif
174300100318     C                   enddo
174400100318     C                   endif
174500100318     C*
174600100318     C                   ENDSR
174700100318     C***
174800070326
174900100326
175000100326
175100100326     C*----------------------------------------------------*
175200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175300100326     C*----------------------------------------------------*
175400100326     C     REPNSP        BEGSR
175500100326     C*
175600100326     C* NSP => Stacco un numeratore da AZNUM
175700100326     C                   clear                   TRUL33DS
175800100326     C                   eval      I33OPE = *zeros
175900100326     C                   eval      I33CNU = 302
176000100326     C                   eval      I33NUM = 1
176100100326     C                   movel     TRUL33DS      KPJBU
176200100326     C                   call      'TRUL33R'
176300100326     C                   parm                    KPJBA
176400100326     C                   movel     KPJBU         TRUL33DS
176500100326     C                   if        O33ERR = *zeros
176600100326     C                   z-add     O33NRF        VABNSP
176700100326     C                   z-add     O33NRF        VATNSP
176800100326     C                   else
176900100326     C                   SETON                                        31
177000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177100100326     C                             + ' ' + 'VABNSP VATNSP'
177200100326     C                   endif
177300100326     C*
177400100326     C                   ENDSR
177500010330
177600100326
177700010330
177800000613     C     *inzsr        BEGSR
177900990910     C*
178000990910     C     *entry        plist
178100990920     C                   parm                    tivlrds
178200990921     C                   parm      wrkesito      esito
178300000724     C                   parm                    prmlit
178400000710     C                   parm                    prmfir
178500010330     C*
178600010330     C* CALCOLA LA DATA CORRENTE
178700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
178800100318     C                   eval      datcor = %dec(%date() : *ISO)
178900000613     C*
179000000613     C                   ENDSR
179100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179200130703X;X;X;X;X;X;X;X;X;X;X;VABRSD;X;X;X;X;X;VABLOD;X;VABIND;VABNZD;X;VABCAD;X;X;VABRMN;
179300130528X;X;W_VABNSP;X;X;
179400130528
179500111116
179600100318
