000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120604      *
000300120604      * PARTICOLARITA':
000400130328      * Siccome ci sono dei dati come la provincia e il fermo deposito che arrivano in minuscolo,
000500130328      * trasformo tutti i dati in maiuscolo appena letto TIVIN.
000600130328      * Per la nazione consideriamo il dato come la nazione ISO e poi la sostituiamo con la nostra.
000700130328      * Se la nazione è straniera, la tariffa è 300.
000800120604
000900121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller) BNDDIR('UBBNDDIR')
001000120209     F*
001100990910     Ftivin00r  uF   E             DISK    usropn
001200100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001300040119     FFIVATwwr  O    E             DISK    usropn
001400130328     Ftabel00f  if   e           k disk
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
002700130328     D ds15          e ds
002800040128     D tisi95ds      e ds
002900100413     D fivabds       e ds                  extname(fivab00f)
003000100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003100100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003200010330     D esito           s              1
003300010330     D prmlit          s             10
003400010330     D prmfir          s             10
003500010330     D wrkesito        s                   like(esito)
003600010330     D rrnum           s              6  0 INZ(*zeros)
003700130328     D jNAZ            s              5  0 INZ(*zeros)
003800040510     D parccm          s              8    INZ(*blanks)
003900040119     D parmbr          s             10    INZ(*blanks)
004000040119     D paropz          s              1    INZ(*blanks)
004100040119     D chkcall         s              1    INZ(*blanks)
004200080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004300080418     D wFlgCAS         s              1    INZ(*blanks)
004400120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004500120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004600130122     D Intestazioni    s               n   inz(*on)
004700100326     D*------------------
004800100326     D* DS REPERIMENTO NUMERATORE
004900100326     D*------------------
005000100326     D trul33ds      e ds                  inz
005100100326     D*------------------
005200100326     D* DS ARCHITETTURA
005300100326     D*------------------
005400100326     D kpjba         e ds                  inz
005500100326
005600030715     D*------------------
005700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005800030715     D*------------------
005900030715     D SkSplitFLD      S             10    DIM(100)
006000030715     D SkSplitCSV      S            256    DIM(100)
006100030715     D CharCSV         S              1
006200030715     D CharTXT         S              1
006300030715     D CharNUM         S              1
006400100318     D CharSOS         S              1
006500030715     D posDa           S              3  0 INZ(*zeros)
006600030715     D posA            S              3  0 INZ(*zeros)
006700030715     D i               s              3  0 INZ(1)
006800030715     D wGiro           s              1  0 INZ(*zeros)
006900070823     D wVATNOT_A       s                   LIKE(VATNOT)
007000060331     D wVATNOT_B       s                   LIKE(VATNOT)
007100060331     D wVATNOT_E       s                   LIKE(VATNOT)
007200110927     D wVATNOT_I       s                   LIKE(VATNOT)
007300120411     D wNOT            s             70
007400130222     D w70             s             70
007500130222     D wRSD            s             35
007600130222     D wRD2            s             35
007700120411     D Data8A          s              8
007800120709     D wBarcode        s             11
007900120709     D wBarcodeN       s             11s 0
008000120629     D wX              s              3s 0
008100030822     D*------------------
008200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008300030822     D*------------------
008400030822     D posDaDft        S              3  0 INZ(*zeros)
008500030822     D posADft         S              3  0 INZ(*zeros)
008600030822     D j               s              3  0 INZ(1)
008700030822     D wGiroDft        s              1  0 INZ(*zeros)
008800130328     D*------------
008900130328     D* SCHIERE
009000130328     D*------------
009100130328     D skNAZISO        S              3    DIM(1000)
009200130328     D skNAZBAR        S              3    DIM(1000)
009300080418     D*------------------
009400080418     D* Costanti
009500080418     D*------------------
009600080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009700080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009800100318
009900100318     D*------------------
010000100318     D* LINKING A DEFINIZIONI ESTERNE
010100100318     D*------------------
010200100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010300100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010400010330
010500000913     C                   reset                   rrnum
010600990921     C                   reset                   esito
010700990921     C                   reset                   wrkesito
010800010601     C*
010900130328     C                   EXSR      CARTAB                                       CARICA TABELLE
011000010601     C                   exsr      opeini
011100010605     C                   exsr      rwvab
011200010601     C*
011300010601     C                   seton                                        lr
011400010601
011500010601
011600010601
011700010601     C*--------------------------------------------------------
011800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011900010601     C*--------------------------------------------------------
012000010601     C     PREELA        BEGSR
012100010601     C*
012200010601     C* SE OCCORRE SPEDIRE IN FILIALE
012300010601     C                   if        invfil <> *zeros and
012400010601     C                             flgGiro = '0'
012500010601     C*
012600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012700010601     C                   eval      flgGiro = '1'
012800010601     C*
012900010601     C                   endif
013000010601     C*
013100010601     C                   ENDSR
013200010601     C***
013300010601
013400010601
013500010601
013600010601
013700010601     C*--------------------------------------------------------
013800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013900010601     C*--------------------------------------------------------
014000010601     C     ENDELA        BEGSR
014100040119     C*
014200040119     C* Effettuo la chiamata al CLLE preposto
014300040119     C                   call(e)   'TITVVTC'
014400040510     C                   parm                    parccm
014500040119     C                   parm                    parmbr
014600040119     C                   parm      '2'           paropz
014700050201     C*
014800050201     C* Effettuo lancio TISI95 solo x chiusura
014900050201     C                   CLEAR                   TISI95DS
015000050201     C                   EVAL      I95TLA = 'C'
015100050201     C                   CALL      'TISI95R'
015200050201     C                   PARM                    TISI95DS
015300000616     C*
015400010601     C                   ENDSR
015500010601
015600010601
015700010330     C*--------------------------------------------------------
015800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015900010330     C*--------------------------------------------------------
016000010605     C     RWVAB         BEGSR
016100010330     C*
016200010330     C                   if        not %open(tivin00r)
016300010330     C                   open      tivin00r
016400010330     C                   endif
016500021025     C                   if        not %open(fivabwwr)
016600021025     C                   open      fivabwwr
016700010330     C                   endif
016800040119     C*
016900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017000040119     C                   exsr      prevat
017100040119     C*
017200040119     C                   if        chkcall = '0'
017300040119     C*
017400040119     C                   if        not %open(fivatwwr)
017500040119     C                   open      fivatwwr
017600040119     C                   endif
017700010330     C*
017800040119     C                   clear                   §CTROKVB          7 0
017900040119     C                   clear                   §CTROKVT          7 0
018000040119     C                   clear                   §CTRMO            7 0
018100040119     C                   clear                   §CTRNO            7 0
018200120410     C*
018300120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018400120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018500120410     C                   IF        Intestazioni = *off
018600120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018700120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018800120410     C                   exsr      inzvar
018900120410     C                   exsr      impvab
019000120410     C                   ENDIF
019100010330     C*
019200030822     C                   do        *HIVAL
019300010330     C*
019400030822     C                   read      tivin00r                               70
019500130322     C*
019600130322     C* trasformo tutti i dati in maiuscolo
019700130322     C                   EVAL      VINDTA = %xlate(minu:maiu:VINDTA)
019800130322     C*
019900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020000010618     C                   if        vinflg = '1'
020100010618     C                   eval      flgOk = '1'
020200010618     C                   endif
020300040510     C*
020400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020500040510     C                   z-add     *zeros        wLenVINDTA        4 0
020600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021000040510     C                   z-add     1             wFlgVINDTA
021100040510     C                   leave
021200040510     C                   endif
021300040510     C                   enddo
021400010618     C*
021500040510     C                   if        vindta > *blanks AND
021600040510     C                             wFlgVINDTA = 1
021700010330     C                   add       1             rrnum
021800010330     C*
021900010601     C                   if        *in70 = *off and
022000010330     C                             (vinflg = *blanks
022100010330     C                              or vinflg = '0'
022200010330     C                              or vinflg = '2')
022300010330     C*
022400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022500010711     C                   if        vinflg = *blanks or vinflg = '0'
022600010711     C                   clear                   vinmsg
022700010711     C                   endif
022800010601     C*
022900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
023000070925     C                   eval      vindta  = ' ' + %trim(vindta)
023100070925     C*
023200080527     C                   exsr      inzvar
023300100318     C                   exsr      normCSV
023400010605     C                   exsr      impvab
023500010601     C*
023600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023700010605     C                   eval      depfil = VABLNP
023800010601     C                   exsr      repfil
023900010601     C                   if        depfil = invfil
024000021025     C                   if        vlrpoi = 999
024100030822     C                   move(P)   invfil        VABFGS
024200021025     C                   else
024300030822     C                   move(P)   vlrpoi        VABFGS
024400021025     C                   endif
024500010601     C*
024600010601     C                   exsr      PREELA
024700010601     C*
024800010604     C* Ebbene...
024900010604     C*
025000040119     C  N31              add       1             §CTROKVB
025100040119     C   32              add       1             §CTRMO
025200040119     C   31              add       1             §CTRNO
025300080418     C*
025400030822     C                   if        wGiro = 2
025500080513     C* Scrivo sempre il VAT
025600080513     C  N31              exsr      wrivat
025700110201     C                   exsr      chkvab
025800080513     C                   endif
025900010604     C*
026000010604     C                   if        *in31 = *off and
026100010604     C                             *in32 = *off
026200010604     C                   eval      vinflg = '1'
026300010604     C                   else
026400010604     C                   eval      vinflg = '2'
026500010604     C                   endif
026600010604     C                   endif
026700010601     C*
026800010604     C                   endif
026900010604     C*
027000010330     C                   else
027100010330     C                   eval      vinflg = '1'
027200010330     C                   endif
027300010601     C*
027400010601     C  N70              update    tivin000
027500010330     C*
027600030822     C  N70              enddo
027700040119     C*
027800080527     C* Verifico dati rimasti "in cannna"...
027900080527     C                   seton                                        65
028000110201     C                   exsr      chkvab
028100080527     C*
028200040119     C                   endif                                                  (endif - chkcall)
028300010601     C*
028400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028500010601     C                   if        cntNonEl = *zeros or
028600010601     C                             flgMulti = '0'
028700010330     C* Se non ci sono record con errori ...
028800010601     C                   if        §ctrno = 0 and
028900010604     C                             §ctrmo = 0 and
029000010601     C                             flgStato <> '2'
029100010330     C* ... restituisco esito OK.
029200010330     C                   eval      wrkesito = '0'
029300010330     C                   else
029400040119     C                   if        §ctrokvb > 0
029500010330     C                   eval      wrkesito = '1'
029600010330     C                   else
029700010615     C                   if        flgOk = '0'
029800010615     C                   eval      wrkesito = '2'
029900010615     C                   else
030000010615     C                   eval      wrkesito = '6'
030100010615     C                   endif
030200010330     C                   endif
030300010330     C                   endif
030400010601     C                   else
030500010601     C                   eval      wrkesito = '9'
030600010601     C                   endif
030700010330     C*
030800010330     C                   if        %open(tivin00r)
030900010330     C                   close     tivin00r
031000010330     C                   endif
031100021025     C                   if        %open(fivabwwr)
031200021025     C                   close     fivabwwr
031300010330     C                   endif
031400040512     C                   if        %open(fivatwwr)
031500040512     C                   close     fivatwwr
031600040512     C                   endif
031700010601     C*
031800010601     C                   if        vlrpoi <> 999
031900010601     C                   eval      invfil = vlrpoi
032000010601     C                   endif
032100010330     C*
032200040119     C                   if        §ctrokvb > 0
032300010601     C                             and invfil > *zeros
032400010330     C                   exsr      invio
032500010330     C                   endif
032600010601     C*
032700010618     C                   if        flgGiro = '1'
032800010601     C                   exsr      endela
032900010618     C                   endif
033000010330     C*
033100010330     C                   ENDSR
033200010330     C***
033300080527
033400080527
033500080527
033600080527     C*----------------------------------------------------*
033700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033800080527     C*----------------------------------------------------*
033900100413     C     CHKVAB        BEGSR
034000110201     C*
034100110201     C* Se richiesti ignorare colli / peso / volume
034200110315 xxx C***                if        *in35
034300110315 xxx C***                z-add     *zeros        VABNCL
034400110315 xxx C***                z-add     *zeros        VABPKB
034500110315 xxx C***                z-add     *zeros        VABVLB
034600110315 xxx C***                endif
034700080527     C*
034800080527     C* Scrivo il VAB solo a rottura d spedizione
034900080527     C                   if        depspe = *zeros
035000080527     C  N31              eval      fivabds_sav = fivabds
035100080527     C                   eval      depspe = vabNSP
035200080527     C                   else
035300080527     C                   if        depspe = vabNSP AND *in65 = *off
035400120703     C   51              add       VABNCL        sav_VABNCL
035500100413     C   51              add       VABPKB        sav_VABPKB
035600100413     C   51              add       VABVLB        sav_VABVLB
035700100720     C   52              z-add     VABNCL        sav_VABNCL
035800100720     C   52              z-add     VABPKB        sav_VABPKB
035900100720     C   52              z-add     VABVLB        sav_VABVLB
036000100720     C   53              add       VABNCL        sav_VABNCL
036100100720     C   53              z-add     VABPKB        sav_VABPKB
036200100720     C   53              z-add     VABVLB        sav_VABVLB
036300080527     C                   else
036400080527     C  N31              eval      fivabds_ok = fivabds_sav
036500080527     C  N31              exsr      wrivab
036600080527     C  N31              eval      fivabds_sav = fivabds
036700080527     C                   eval      depspe = vabNSP
036800080527     C                   endif
036900080527     C                   endif
037000080527     C*
037100080527     C                   ENDSR
037200040119
037300040119
037400040119
037500040119     C*----------------------------------------------------*
037600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037700040119     C*----------------------------------------------------*
037800040510     C     WRIVAB        BEGSR
037900080513     C*
038000080513     C* Gestisco l'eventuale rottura x numero spedizione
038100080527     C                   if        VABNSP <> depspe or *in65 = *on
038200070301     C                   if        wVATNOT_E <> *blanks
038300070301     C                   eval      VABCTM = '7Q'
038400070301     C                   endif
038500100413     C                   write     fivab000
038600080513     C                   endif
038700040119     C*
038800040119     C                   ENDSR
038900040119
039000040119
039100040119
039200040119     C*----------------------------------------------------*
039300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039400040119     C*----------------------------------------------------*
039500040510     C     WRIVAT        BEGSR
039600060512     C*
039700060512     C* Inizializzo i campi chiave
039800060512     C                   eval      VATFGS = VABFGS
039900060512     C                   eval      VATAAS = VABAAS
040000060512     C                   eval      VATCCM = VABCCM
040100070823     C                   eval      VATNRS = VABNRS
040200060512     C                   eval      VATNSP = VABNSP
040300060512     C                   eval      VATLNP = VABLNP
040400040119     C*
040500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040600040510     C                   if        VATTRC = *blanks
040700040510     C                   eval      VATTRC = 'E'
040800040510     C                   endif
040900110318     C                   if        VATNOT <> *blanks
041000040510     C                   write     FIVAT000
041100040119     C                   add       1             §CTROKVT
041200110318     C                   endif
041300060331     C*
041400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041500060331     C                   if        wVATNOT_A <> *blanks
041600060331     C                   eval      VATNOT = wVATNOT_A
041700060331     C                   eval      VATTRC = 'A'
041800060331     C                   write     FIVAT000
041900060331     C                   add       1             §CTROKVT
042000060331     C                   endif
042100060331     C*
042200060331     C                   if        wVATNOT_B <> *blanks
042300060331     C                   eval      VATNOT = wVATNOT_B
042400060331     C                   eval      VATTRC = 'B'
042500060331     C                   write     FIVAT000
042600060331     C                   add       1             §CTROKVT
042700060331     C                   endif
042800060331     C*
042900060331     C                   if        wVATNOT_E <> *blanks
043000060331     C                   eval      VATTRC = 'E'
043100120629     C*
043200130320     C* NON sviluppo i "CHI SONO" perché spedizione monocollo
043300120703     C*
043400120711     C                   EVAL      VATNOT = wVATNOT_E
043500120629     C                   write     FIVAT000
043600120629     C                   add       1             §CTROKVT
043700120604     C*
043800120604     C                   endif
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
063300120418     C                   Z-ADD     PiVal         VABPKB
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
094400130222     C* Stacco un progressivo x numero spedizione
094500130222     C                   EXSR      repNSP
094600100326     C*
094700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094800030715     C                   EVAL      i = 1
094900030715     C                   DOW       i < %elem(SkSplitFLD)
095000030715     C*
095100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095300030715     C                             SkSplitCSV(i) <> *blanks
095400030721     C*** ATB
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095600030721     C                   EVAL      VABATB = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** RSD
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096300030715     C* ==
096400030715     C                   ENDIF
096500030721     C*** RD2
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030715     C*** IND
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097100030715     C                   EVAL      VABIND = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** LOD
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030715     C*** PRD
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098000030715     C                   ENDIF
098100030721     C*** NZD
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030715     C*** RMA
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** NOT
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** NT2
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** VCA
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030715     C*** VAS
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030715     C*** FFD
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030721     C*** GC1
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** GC2
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** TSP
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102000030721     C                   ENDIF
102100040510     C*** CBO
102200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102400040510     C                   ENDIF
102500030721     C*** NAS
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TIC
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** GCA
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** XCO
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** CTM
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** TCR
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** CTS
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** FTM
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** VAD
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** GMA
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** GGA
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** GVA
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TC1
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** TC2
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** SCL
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** RMO
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** NMO
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*
109400030721     C* Reperisco quindi i campi numerici...
109500040510     C*** NSP
109600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109700040510     C                   EVAL      PiStr=SkSplitCSV(i)
109800040510     C                   EXSR      CHKNUM
109900040510     C                   IF        PiInt=*on
110000040510     C                   Z-ADD     PiVal         VABNSP
110100040510     C                   ELSE
110200040510     C                   SETON                                        31
110300040510     C                   EVAL      VABNSP = *zeros
110400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110500040510     C                             + ' ' + 'VABNSP'
110600040510     C                   ENDIF
110700040510     C                   ENDIF
110800071106     C*** RMN
110900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111000071106     C                   EVAL      PiStr=SkSplitCSV(i)
111100071106     C                   EXSR      CHKNUM
111200071106     C                   IF        PiInt=*on
111300071106     C                   Z-ADD     PiVal         VABRMN
111400071106     C                   ELSE
111500071106     C                   SETON                                        32
111600071106     C                   EVAL      VABRMN = *zeros
111700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111800071106     C                             + ' ' + 'VABRMN'
111900071106     C                   ENDIF
112000071106     C                   ENDIF
112100030721     C*** CAD
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on
112600030721     C                   Z-ADD     PiVal         Num5_0
112700030721     C                   MOVEL(p)  Num5_0        VABCAD
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000030721     C                   EVAL      VABCAD = *zeros
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABCAD'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** NCL
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on
114000030721     C                   Z-ADD     PiVal         VABNCL
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      VABNCL = *zeros
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500040122     C                             + ' ' + 'VABNCL'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** PKB
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiNum=*on
115300120418     C                   Z-ADD(H)  PiVal         VABPKB
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      VABPKB = *zeros
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABPKB'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** CAS
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116300110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116400111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
116500120713     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
116600120713     C                             %trim(SkSplitCSV(i)) <> '0,00'
116700120605     C                   EVAL      VABVCA = 'EUR'
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
125800130222     C                   EVAL      vabVAS = 'EUR'
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABIAS'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** VLB
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiNum=*on
127000030721     C                   Z-ADD     PiVal         VABVLB
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABVLB'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** QFT
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiNum=*on
128200030721     C                   Z-ADD     PiVal         VABQFT
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABQFT'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** NCD
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on      AND
129400030721     C                             PiVal<=9999999
129500030721     C                   Z-ADD     PiVal         VABNCD
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABNCD'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** NCA
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on      AND
130700030721     C                             PiVal<=9999999
130800030721     C                   Z-ADD     PiVal         VABNCA
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABNCA'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** ZNC
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on AND
132000030721     C                             PiVal<=99
132100030721     C                   Z-ADD     PiVal         VABZNC
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABZNC'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** DCR
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on       AND
133300030721     C                             PiVal<=99999999
133400030721     C                   Z-ADD     PiVal         VABDCR
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABDCR'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** HCR
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on   AND
134600030721     C                             PiVal<=9999
134700030721     C                   Z-ADD     PiVal         VABHCR
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABHCR'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** VMD
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiNum=*on
135900030721     C                   Z-ADD     PiVal         VABVMD
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABVMD'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** ANT
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on        AND
137100030721     C                             PiVal<=999999999
137200030721     C                   Z-ADD     PiVal         VABANT
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137600030721     C                             + ' ' + 'VABANT'
137700030721     C                   ENDIF
137800030721     C                   ENDIF
137900030721     C*** CMO
138000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138100030721     C                   EVAL      PiStr=SkSplitCSV(i)
138200030721     C                   EXSR      CHKNUM
138300030721     C                   IF        PiInt=*on
138400030721     C                   Z-ADD     PiVal         Num5_0
138500030721     C                   MOVEL(p)  Num5_0        VABCMO
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800030721     C                   EVAL      VABCMO = *zeros
138900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139000030721     C                             + ' ' + 'VABCMO'
139100030721     C                   ENDIF
139200030721     C                   ENDIF
139300040510     C*
139400040510     C* Reperisco i campi del FIVAT
139500040510     C*** VATNOT
139600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139800040510     C                   ENDIF
139900040510     C*** VATTRC
140000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140200040510     C                   ENDIF
140300060331     C*** VATNOT_A
140400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140600060331     C                   ENDIF
140700060331     C*** VATNOT_B
140800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141000060331     C                   ENDIF
141100060331     C*** VATNOT_E
141200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141400060331     C                   ENDIF
141500110927     C*** VATNOT_I
141600110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141700110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
141800110927     C                   ENDIF
141900070823     C*
142000120713 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
142100130322
142200130322     C*** W_VABRMN contiene sia RMN che RMA
142300130322     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142400130322
142500130322     C                   EVAL      VABRMA = SkSplitCSV(i)
142600130322
142700130322     C                   EVAL      PiStr=SkSplitCSV(i)
142800130322     C                   EXSR      CHKNUM
142900130322     C                   IF        PiInt=*on
143000130322     C                   Z-ADD     PiVal         VABRMN
143100130322     C                   ELSE
143200130322     C                   SETON                                        32
143300130322     C                   EVAL      VABRMN = *zeros
143400130322     C                   EVAL      vinmsg = %trimr(vinmsg)
143500130322     C                             + ' ' + 'VABRMN'
143600130322     C                   ENDIF
143700130322
143800130322     C                   ENDIF
143900120713
144000130322     C*** W_VABFFD Fermo deposito solo se contiene 'SI'
144100130322     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABFFD'
144200130322     C* non c'è bisogno dell'xlate avendola già fatta su tutto VINDTA
144300130322     C***                IF        %xlate(minu:maiu:SkSplitCSV(i))='SI'
144400130322     C                   IF        SkSplitCSV(i)='SI'
144500130322     C                   EVAL      VABFFD = 'S'
144600130322     C                   ENDIF
144700130322     C                   ENDIF
144800130328
144900130328     C* per la nazione consideriamo il dato come la nazione ISO e poi la
145000130328     C* sostituiamo con la nostra
145100130328     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
145200130328     C                   Z-ADD     1             jNAZ
145300130328     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
145400130328     C                   IF        %found
145500130328     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
145600130328     C* se la nazione è straniera
145700130328     C                   IF        VABNZD <> *blank
145800130328     C* la tariffa è 300
145900130328     C                   EVAL      VABCTR = 300
146000130328     C                   ENDIF
146100130328     C                   ENDIF
146200130328     C                   ENDIF
146300120301
146400070925     C*
146500071029     C***  <===  -----------------------------------------
146600040510     C*
146700030715     C                   ENDIF
146800030715     C                   EVAL      i = i + 1
146900030715     C                   ENDDO
147000070925     C*
147100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147200120411     C*
147300130320     C*** VABRMN è = la parte numerica di VABRMA dopo lo /
147400130320     C                   EVAL      VABRMN =
147500130320     C                              %dec(%subst(VABRMA : %scan('/':VABRMA)+1:
147600130320     C                              %len(%trim(VABRMA))-%scan('/':VABRMA)+1)
147700130320     C                              : 15 : 0)
147800070925     C*
147900071029     C***  <===  -----------------------------------------
148000070925     C*
148100070925     C* Considerazioni finali su CBO/CAS
148200100318     C                   IF        wFlgCAS = '1'    OR
148300100318     C                             VABCAS  > *zeros
148400070925     C                   IF        VABCBO = '1'
148500070925     C                   EVAL      VABCBO = '4'
148600070925     C                   ENDIF
148700070925     C                   IF        VABCBO = '2'
148800070925     C                   EVAL      VABCBO = '6'
148900070925     C                   ENDIF
149000070925     C                   ENDIF
149100070925     C***  <===  ----------------------------
149200040510     C*
149300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149400040510     C                   IF        VABNSP = *zeros
149500040510     C                   SETON                                        31
149600040510     C                   ENDIF
149700040715     C*
149800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149900040715     C                   IF        VABLNP = *zeros
150000040715     C                   SETON                                        31
150100040715     C                   ENDIF
150200040510     C*
150300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150400120411     C                   IF        VABRMN = *zeros
150500120411     C                   Z-ADD     VABNSP        VABRMN
150600120411     C                   ENDIF
150700120328     C* IN QUESTO CASO LO FORZO = 1
150800120411     C***                IF        VABRMN = *zeros
150900120411     C***                Z-ADD     1             VABRMN
151000120411     C***                ENDIF
151100040510     C*
151200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151300040510     C                   IF        VABRMA = *blanks
151400040714     C                   MOVEL     VABRMN        VABRMA
151500040510     C                   ENDIF
151600040128     C*
151700040510     C* Se provincia nn valorizzata la reperisco
151800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151900110912     C                   IF        VABPRD =  *blanks AND
152000110912     C                             VABNZD =  *blanks AND
152100110912     C                             VABCAD <> *blanks AND
152200110912     C                             VABLOD <> *blanks
152300040128     C                   CLEAR                   TISI95DS
152400040128     C                   EVAL      I95TCN = '3'
152500040128     C                   Z-ADD     datcor        I95DAT
152600040128     C                   EVAL      I95NAR = VABNZD
152700040128     C                   EVAL      I95CAP = VABCAD
152800040128     C                   EVAL      I95LOC = VABLOD
152900040128     C                   CALL      'TISI95R'
153000040128     C                   PARM                    TISI95DS
153100040128     C                   EVAL      VABPRD = O95PRV
153200040510     C                   ENDIF
153300020204     C*
153400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153500020204     C                   EXSR      CHKIMPDIV
153600030715     C                   ENDIF
153700020204     C*
153800010607     C                   ENDSR
153900010607     C*----------------------------------------------------*
154000010601
154100020204
154200020204     C*----------------------------------------------------*
154300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154400020204     C*----------------------------------------------------*
154500020204     C     CHKIMPDIV     BEGSR
154600020204     C*
154700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154800020204     C                   Z-ADD     *zeros        wrkDec            9 9
154900020204     C*
155000020204     C* Come prima cosa effettuo considerazioni sulla divisa
155100020204     C                   IF        vabIAS > *zeros
155200020204     C                   IF        vabVAS <> 'EUR'
155300020204     C                   EVAL      vabVAS =  'ITL'
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C                   IF        vabCAS > *zeros
155800020204     C                   IF        vabVCA <> 'EUR'
155900020204     C                   EVAL      vabVCA =  'ITL'
156000020204     C                   ENDIF
156100020204     C                   ENDIF
156200020204     C*
156300020204     C                   IF        vabVMD > *zeros
156400020204     C                   IF        vabVAD <> 'EUR'
156500020204     C                   EVAL      vabVAD =  'ITL'
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157000020204     C                   Z-ADD     vabIAS        wrkDec
157100020204     C                   IF        wrkDec > *zeros
157200020204     C                   IF        vabVAS = 'ITL'
157300020204     C                   EVAL      vabIAS = *zeros
157400020204     C                   ENDIF
157500020204     C                   ENDIF
157600020204     C*
157700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157800020204     C                   Z-ADD     vabCAS        wrkDec
157900020204     C                   IF        wrkDec > *zeros
158000020204     C                   IF        vabVCA = 'ITL'
158100020204     C                   EVAL      vabCAS = *zeros
158200020204     C                   ENDIF
158300020204     C                   ENDIF
158400020204     C*
158500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158600020204     C                   Z-ADD     vabVMD        wrkDec
158700020204     C                   IF        wrkDec > *zeros
158800020204     C                   IF        vabVAD = 'ITL'
158900020204     C                   EVAL      vabVMD = *zeros
159000020204     C                   ENDIF
159100020204     C                   ENDIF
159200020204     C*
159300020204     C                   ENDSR
159400020204     C***
159500010330
159600100318
159700100318
159800100318     C*----------------------------------------------------*
159900100318     C*  CONTROLLO NUMERICITA' CAMPI
160000100318     C*----------------------------------------------------*
160100100318     C     CHKNUM        BEGSR
160200100318     C*
160300100318     C                   IF        PiDecChr = *blanks
160400100318     C                   EVAL      PiDecChr = CharNUM
160500100318     C                   ENDIF
160600100318     C*
160700100318     C                   callp     UBISNUM_Check(PiStr
160800100318     C                                          :PiDecChr
160900100318     C                                          :PiVal
161000100318     C                                          :PiNum
161100100318     C                                          :PiInt)
161200100318     C*
161300100318     C                   ENDSR
161400100318     C***
161500010601
161600010601
161700010601
161800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161900010601     C     repfil        BEGSR
162000010601     C*
162100010601     C                   if        invfil = *zeros and
162200010601     C                             depfil > *zeros and
162300010629     C                             (vinflg = *blanks or
162400010629     C                              vinflg = *zeros)
162500010601     C
162600010601     C                   eval      invfil = depfil
162700010601     C                   endif
162800010601     C*
162900010601     C                   if        depfil <> invfil and
163000010601     C                             invfil > *zeros
163100010601     C                   eval      flgMulti = '1'
163200010601     C                   if        vinflg = *blanks
163300010601     C                   add       1             cntNonEl
163400010601     C                   endif
163500010601     C                   endif
163600010601     C*
163700010601     C                   if        vinflg = '2'
163800010601     C                   eval      flgStato = '2'
163900010601     C                   endif
164000010601     C*
164100010601     C                   ENDSR
164200010601     C***
164300010601
164400010601
164500010601
164600010330
164700040119      /TITLE Invio dei dati al punto operativo.
164800040119     C     invio         BEGSR
164900040119     C*
165000040119     C* 1° invio FIVAT
165100040119     C                   reset                   dscmz
165200040119     C                   move      vlrpoi        cmzdst
165300040119     C                   eval      cmzfld = 'FIVATWWR'
165400040119     C                   eval      cmzmbd = vlrhdl
165500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165600040119     C***                if        prmfir = *blanks
165700040119     C                   eval      cmzfla = 'FIVAT00F'
165800040119     C                   eval      cmzmba = 'FIVAT00F'
165900040119     C***                else
166000040119     C***                eval      cmzfla = prmfir
166100040119     C***                eval      cmzmba = prmfir
166200040119     C***                endif
166300040119     C                   eval      cmznrr = *zeros
166400040119     C                   move      §ctrokvt      cmznrr
166500040119     C                   eval      cmzlba = vlrfl1
166600040119     C                   call(e)   'TIS711C'
166700040119     C                   parm                    dscmz
166800040119     C                   parm      *blanks       esito
166900040119     C                   if        %error
167000040119     C                             or cmzerr = '1'
167100040119     C                             or esito  = '1'
167200040119     C                   eval      wrkesito = '3'
167300040119     C                   else
167400040119     C*
167500040119     C* 2° invio FIVAB
167600040119     C                   reset                   dscmz
167700040119     C                   move      vlrpoi        cmzdst
167800040119     C                   eval      cmzfld = vlrfou
167900040119     C                   eval      cmzmbd = vlrhdl
168000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168100040119     C***                if        prmfir = *blanks
168200040119     C                   eval      cmzfla = 'FIVAB00F'
168300040119     C                   eval      cmzmba = 'FIVAB00F'
168400040119     C***                else
168500040119     C***                eval      cmzfla = prmfir
168600040119     C***                eval      cmzmba = prmfir
168700040119     C***                endif
168800040119     C                   eval      cmznrr = *zeros
168900040119     C                   move      §ctrokvb      cmznrr
169000040119     C                   eval      cmzlba = vlrfl1
169100040119     C                   call(e)   'TIS711C'
169200040119     C                   parm                    dscmz
169300040119     C                   parm      *blanks       esito
169400040119     C                   if        %error
169500040119     C                             or cmzerr = '1'
169600040119     C                             or esito  = '1'
169700040119     C                   eval      wrkesito = '3'
169800040119     C                   endif
169900040119     C                   endif
170000040119     C*
170100040119     C                   ENDSR
170200040119     C***
170300130328
170400130328     C*--------------------------------------------------------
170500130328     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
170600130328     C*--------------------------------------------------------
170700130328     C     CARTAB        BEGSR
170800130328     C*
170900130328     C* TABELLA '15' - NAZIONI
171000130328     C                   clear                   skNAZISO
171100130328     C                   clear                   skNAZBAR
171200130328     C                   eval      tblKUT = 1
171300130328     C                   eval      tblCOD = '15'
171400130328     C     KEYtabP       setll     tabel00f
171500130328     C     KEYtabP       reade     tabel00f
171600130328     C                   dow       not %eof(tabel00f)
171700130328     C                   if        tblFLG = *blanks
171800130328     C                   movel(p)  tblUNI        ds15
171900130328     C                   add       1             jNAZ
172000130328     C                   eval      skNAZBAR(jNAZ) = tblKEY
172100130328     C                   eval      skNAZISO(jNAZ) = §15COD
172200130328     C                   endif
172300130328     C     KEYtabP       reade     tabel00f
172400130328     C                   enddo
172500130328     C*
172600130328     C                   ENDSR
172700130328     C***
172800010601
172900010601      /TITLE Invio dei dati al punto operativo.
173000010601     C     opeini        BEGSR
173100010601     C*
173200010601     C* Inizializzo flag e contatori operativi
173300010601     C                   movel     '0'           flgGiro           1
173400010601     C                   movel     '0'           flgMulti          1
173500010601     C                   movel     '1'           flgStato          1
173600010615     C                   movel     '0'           flgOk             1
173700010601     C                   z-add     *zeros        cntNonEl         10 0
173800010601     C                   z-add     *zeros        depfil            3 0
173900010601     C                   z-add     *zeros        invfil            3 0
174000010601     C*
174100010601     C                   ENDSR
174200010601     C***
174300070326
174400070326
174500070326
174600070326
174700070326     C     *pssr         BEGSR
174800070329     C*
174900070329     C                   if        %open(tivin00r)
175000070329     C                   close     tivin00r
175100070329     C                   endif
175200070329     C                   if        %open(fivabwwr)
175300070329     C                   close     fivabwwr
175400070329     C                   endif
175500070329     C                   if        %open(fivatwwr)
175600070329     C                   close     fivatwwr
175700070329     C                   endif
175800070326     C*
175900070326     C* Effettuo la chiamata al CLLE preposto
176000070326     C                   call(e)   'TITVVTC'
176100070326     C                   parm                    parccm
176200070326     C                   parm                    parmbr
176300070326     C                   parm      '2'           paropz
176400070326     C*
176500070326     C                   eval      wrkesito = '2'
176600070404     C*
176700070404     C                   seton                                        LR
176800070326     C*
176900070326     C                   ENDSR     '*CANCL'
177000070326     C***
177100100318
177200100318
177300100318
177400100318     C     normCSV       BEGSR
177500100318     C*
177600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177700100318     C* separatori campi
177800100318     C                   if        CharTXT <> *blanks
177900100318     C                   z-add     *zeros        wLenVINDTA        4 0
178000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
178100100318     C                   setoff                                       80
178200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
178300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
178400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
178500100318     C                   if        not *in80
178600100318     C                   seton                                        80
178700100318     C                   else
178800100318     C                   setoff                                       80
178900100318     C                   endif
179000100318     C                   endif
179100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
179200100318     C                             and *in80 = *on
179300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
179400100318     C                   endif
179500100318     C                   enddo
179600100318     C                   endif
179700100318     C*
179800100318     C                   ENDSR
179900100318     C***
180000070326
180100100326
180200100326
180300100326     C*----------------------------------------------------*
180400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
180500100326     C*----------------------------------------------------*
180600100326     C     REPNSP        BEGSR
180700100326     C*
180800100326     C* NSP => Stacco un numeratore da AZNUM
180900100326     C                   clear                   TRUL33DS
181000100326     C                   eval      I33OPE = *zeros
181100100326     C                   eval      I33CNU = 302
181200100326     C                   eval      I33NUM = 1
181300100326     C                   movel     TRUL33DS      KPJBU
181400100326     C                   call      'TRUL33R'
181500100326     C                   parm                    KPJBA
181600100326     C                   movel     KPJBU         TRUL33DS
181700100326     C                   if        O33ERR = *zeros
181800100326     C                   z-add     O33NRF        VABNSP
181900100326     C                   z-add     O33NRF        VATNSP
182000100326     C                   else
182100100326     C                   SETON                                        31
182200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
182300100326     C                             + ' ' + 'VABNSP VATNSP'
182400100326     C                   endif
182500100326     C*
182600100326     C                   ENDSR
182700010330
182800100326
182900010330
183000000613     C     *inzsr        BEGSR
183100990910     C*
183200990910     C     *entry        plist
183300990920     C                   parm                    tivlrds
183400990921     C                   parm      wrkesito      esito
183500000724     C                   parm                    prmlit
183600000710     C                   parm                    prmfir
183700010330     C*
183800010330     C* CALCOLA LA DATA CORRENTE
183900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
184000100318     C                   eval      datcor = %dec(%date() : *ISO)
184100130328     C*
184200130328     C* Chiave su TABEL00F - parziale
184300130328     C     KEYtabP       KLIST
184400130328     C                   KFLD                    tblKUT
184500130328     C                   KFLD                    tblCOD
184600000613     C*
184700000613     C                   ENDSR
184800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
184900130328VATNOT_E;W_VABRMN;VABRSD;VATNOT_I;VABIND;VATNOT_B;VABCAD;VABLOD;VABPRD;W_VABNZD;VABPKB;VABVLB;
185000130328VABNCL;VABIAS;VABCAS;W_VABFFD;
185100120604
185200130322
185300120604
