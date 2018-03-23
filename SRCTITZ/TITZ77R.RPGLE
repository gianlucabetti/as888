000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120604      *
000300120604      * PARTICOLARITA':
000400130402      * Siccome ci sono dei dati come il porto che arrivano in minuscolo,
000500130328      * trasformo tutti i dati in maiuscolo appena letto TIVIN.
000600130402      * Se c/assegno, il tipo incasso è TM = assegno .... intestato al mittente
000700120604
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller) BNDDIR('UBBNDDIR')
000900120209     F*
001000990910     Ftivin00r  uF   E             DISK    usropn
001100100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001200040119     FFIVATwwr  O    E             DISK    usropn
001300130328     Ftabel00f  if   e           k disk
001400000313     D*
001500040113     D*------------
001600040113     D* SCHIEDE A PROGRAMMA
001700040113     D*------------
001800080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001900010330     D*----------------------------------------------------
002000010330     D* DICHIARAZIOINE VARIABILI DI WRK
002100010330     D*----------------------------------------------------
002200010330     D dscmz         e ds                  inz
002300010330     D psds           sds
002400010330     D  procname         *PROC
002500010330     D tivlrds       e ds                  extname(tivlr00f)
002600130328     D ds15          e ds
002700040128     D tisi95ds      e ds
002800100413     D fivabds       e ds                  extname(fivab00f)
002900100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003000100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600130328     D jNAZ            s              5  0 INZ(*zeros)
003700040510     D parccm          s              8    INZ(*blanks)
003800040119     D parmbr          s             10    INZ(*blanks)
003900040119     D paropz          s              1    INZ(*blanks)
004000040119     D chkcall         s              1    INZ(*blanks)
004100080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004200080418     D wFlgCAS         s              1    INZ(*blanks)
004300120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004400120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004500130122     D Intestazioni    s               n   inz(*on)
004600100326     D*------------------
004700100326     D* DS REPERIMENTO NUMERATORE
004800100326     D*------------------
004900100326     D trul33ds      e ds                  inz
005000100326     D*------------------
005100100326     D* DS ARCHITETTURA
005200100326     D*------------------
005300100326     D kpjba         e ds                  inz
005400100326
005500030715     D*------------------
005600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005700030715     D*------------------
005800030715     D SkSplitFLD      S             10    DIM(100)
005900030715     D SkSplitCSV      S            256    DIM(100)
006000030715     D CharCSV         S              1
006100030715     D CharTXT         S              1
006200030715     D CharNUM         S              1
006300100318     D CharSOS         S              1
006400030715     D posDa           S              3  0 INZ(*zeros)
006500030715     D posA            S              3  0 INZ(*zeros)
006600030715     D i               s              3  0 INZ(1)
006700030715     D wGiro           s              1  0 INZ(*zeros)
006800070823     D wVATNOT_A       s                   LIKE(VATNOT)
006900060331     D wVATNOT_B       s                   LIKE(VATNOT)
007000060331     D wVATNOT_E       s                   LIKE(VATNOT)
007100110927     D wVATNOT_I       s                   LIKE(VATNOT)
007200120411     D wNOT            s             70
007300130222     D w70             s             70
007400130222     D wRSD            s             35
007500130222     D wRD2            s             35
007600120411     D Data8A          s              8
007700120709     D wBarcode        s             11
007800120709     D wBarcodeN       s             11s 0
007900120629     D wX              s              3s 0
008000030822     D*------------------
008100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008200030822     D*------------------
008300030822     D posDaDft        S              3  0 INZ(*zeros)
008400030822     D posADft         S              3  0 INZ(*zeros)
008500030822     D j               s              3  0 INZ(1)
008600030822     D wGiroDft        s              1  0 INZ(*zeros)
008700130328     D*------------
008800130328     D* SCHIERE
008900130328     D*------------
009000130328     D skNAZISO        S              3    DIM(1000)
009100130328     D skNAZBAR        S              3    DIM(1000)
009200080418     D*------------------
009300080418     D* Costanti
009400080418     D*------------------
009500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009700100318
009800100318     D*------------------
009900100318     D* LINKING A DEFINIZIONI ESTERNE
010000100318     D*------------------
010100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010300010330
010400000913     C                   reset                   rrnum
010500990921     C                   reset                   esito
010600990921     C                   reset                   wrkesito
010700010601     C*
010800130328     C                   EXSR      CARTAB                                       CARICA TABELLE
010900010601     C                   exsr      opeini
011000010605     C                   exsr      rwvab
011100010601     C*
011200010601     C                   seton                                        lr
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011800010601     C*--------------------------------------------------------
011900010601     C     PREELA        BEGSR
012000010601     C*
012100010601     C* SE OCCORRE SPEDIRE IN FILIALE
012200010601     C                   if        invfil <> *zeros and
012300010601     C                             flgGiro = '0'
012400010601     C*
012500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012600010601     C                   eval      flgGiro = '1'
012700010601     C*
012800010601     C                   endif
012900010601     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400010601
013500010601
013600010601     C*--------------------------------------------------------
013700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013800010601     C*--------------------------------------------------------
013900010601     C     ENDELA        BEGSR
014000040119     C*
014100040119     C* Effettuo la chiamata al CLLE preposto
014200040119     C                   call(e)   'TITVVTC'
014300040510     C                   parm                    parccm
014400040119     C                   parm                    parmbr
014500040119     C                   parm      '2'           paropz
014600050201     C*
014700050201     C* Effettuo lancio TISI95 solo x chiusura
014800050201     C                   CLEAR                   TISI95DS
014900050201     C                   EVAL      I95TLA = 'C'
015000050201     C                   CALL      'TISI95R'
015100050201     C                   PARM                    TISI95DS
015200000616     C*
015300010601     C                   ENDSR
015400010601
015500010601
015600010330     C*--------------------------------------------------------
015700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015800010330     C*--------------------------------------------------------
015900010605     C     RWVAB         BEGSR
016000010330     C*
016100010330     C                   if        not %open(tivin00r)
016200010330     C                   open      tivin00r
016300010330     C                   endif
016400021025     C                   if        not %open(fivabwwr)
016500021025     C                   open      fivabwwr
016600010330     C                   endif
016700040119     C*
016800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016900040119     C                   exsr      prevat
017000040119     C*
017100040119     C                   if        chkcall = '0'
017200040119     C*
017300040119     C                   if        not %open(fivatwwr)
017400040119     C                   open      fivatwwr
017500040119     C                   endif
017600010330     C*
017700040119     C                   clear                   §CTROKVB          7 0
017800040119     C                   clear                   §CTROKVT          7 0
017900040119     C                   clear                   §CTRMO            7 0
018000040119     C                   clear                   §CTRNO            7 0
018100120410     C*
018200120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018300120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018400120410     C                   IF        Intestazioni = *off
018500120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018600120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018700120410     C                   exsr      inzvar
018800120410     C                   exsr      impvab
018900120410     C                   ENDIF
019000010330     C*
019100030822     C                   do        *HIVAL
019200010330     C*
019300030822     C                   read      tivin00r                               70
019400130322     C*
019500130322     C* trasformo tutti i dati in maiuscolo
019600130322     C                   EVAL      VINDTA = %xlate(minu:maiu:VINDTA)
019700130322     C*
019800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019900010618     C                   if        vinflg = '1'
020000010618     C                   eval      flgOk = '1'
020100010618     C                   endif
020200040510     C*
020300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020400040510     C                   z-add     *zeros        wLenVINDTA        4 0
020500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020900040510     C                   z-add     1             wFlgVINDTA
021000040510     C                   leave
021100040510     C                   endif
021200040510     C                   enddo
021300010618     C*
021400040510     C                   if        vindta > *blanks AND
021500040510     C                             wFlgVINDTA = 1
021600010330     C                   add       1             rrnum
021700010330     C*
021800010601     C                   if        *in70 = *off and
021900010330     C                             (vinflg = *blanks
022000010330     C                              or vinflg = '0'
022100010330     C                              or vinflg = '2')
022200010330     C*
022300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022400010711     C                   if        vinflg = *blanks or vinflg = '0'
022500010711     C                   clear                   vinmsg
022600010711     C                   endif
022700010601     C*
022800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022900070925     C                   eval      vindta  = ' ' + %trim(vindta)
023000070925     C*
023100080527     C                   exsr      inzvar
023200100318     C                   exsr      normCSV
023300010605     C                   exsr      impvab
023400010601     C*
023500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023600010605     C                   eval      depfil = VABLNP
023700010601     C                   exsr      repfil
023800010601     C                   if        depfil = invfil
023900021025     C                   if        vlrpoi = 999
024000030822     C                   move(P)   invfil        VABFGS
024100021025     C                   else
024200030822     C                   move(P)   vlrpoi        VABFGS
024300021025     C                   endif
024400010601     C*
024500010601     C                   exsr      PREELA
024600010601     C*
024700010604     C* Ebbene...
024800010604     C*
024900040119     C  N31              add       1             §CTROKVB
025000040119     C   32              add       1             §CTRMO
025100040119     C   31              add       1             §CTRNO
025200080418     C*
025300030822     C                   if        wGiro = 2
025400080513     C* Scrivo sempre il VAT
025500080513     C  N31              exsr      wrivat
025600110201     C                   exsr      chkvab
025700080513     C                   endif
025800010604     C*
025900010604     C                   if        *in31 = *off and
026000010604     C                             *in32 = *off
026100010604     C                   eval      vinflg = '1'
026200010604     C                   else
026300010604     C                   eval      vinflg = '2'
026400010604     C                   endif
026500010604     C                   endif
026600010601     C*
026700010604     C                   endif
026800010604     C*
026900010330     C                   else
027000010330     C                   eval      vinflg = '1'
027100010330     C                   endif
027200010601     C*
027300010601     C  N70              update    tivin000
027400010330     C*
027500030822     C  N70              enddo
027600040119     C*
027700080527     C* Verifico dati rimasti "in cannna"...
027800080527     C                   seton                                        65
027900110201     C                   exsr      chkvab
028000080527     C*
028100040119     C                   endif                                                  (endif - chkcall)
028200010601     C*
028300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028400010601     C                   if        cntNonEl = *zeros or
028500010601     C                             flgMulti = '0'
028600010330     C* Se non ci sono record con errori ...
028700010601     C                   if        §ctrno = 0 and
028800010604     C                             §ctrmo = 0 and
028900010601     C                             flgStato <> '2'
029000010330     C* ... restituisco esito OK.
029100010330     C                   eval      wrkesito = '0'
029200010330     C                   else
029300040119     C                   if        §ctrokvb > 0
029400010330     C                   eval      wrkesito = '1'
029500010330     C                   else
029600010615     C                   if        flgOk = '0'
029700010615     C                   eval      wrkesito = '2'
029800010615     C                   else
029900010615     C                   eval      wrkesito = '6'
030000010615     C                   endif
030100010330     C                   endif
030200010330     C                   endif
030300010601     C                   else
030400010601     C                   eval      wrkesito = '9'
030500010601     C                   endif
030600010330     C*
030700010330     C                   if        %open(tivin00r)
030800010330     C                   close     tivin00r
030900010330     C                   endif
031000021025     C                   if        %open(fivabwwr)
031100021025     C                   close     fivabwwr
031200010330     C                   endif
031300040512     C                   if        %open(fivatwwr)
031400040512     C                   close     fivatwwr
031500040512     C                   endif
031600010601     C*
031700010601     C                   if        vlrpoi <> 999
031800010601     C                   eval      invfil = vlrpoi
031900010601     C                   endif
032000010330     C*
032100040119     C                   if        §ctrokvb > 0
032200010601     C                             and invfil > *zeros
032300010330     C                   exsr      invio
032400010330     C                   endif
032500010601     C*
032600010618     C                   if        flgGiro = '1'
032700010601     C                   exsr      endela
032800010618     C                   endif
032900010330     C*
033000010330     C                   ENDSR
033100010330     C***
033200080527
033300080527
033400080527
033500080527     C*----------------------------------------------------*
033600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033700080527     C*----------------------------------------------------*
033800100413     C     CHKVAB        BEGSR
033900110201     C*
034000110201     C* Se richiesti ignorare colli / peso / volume
034100110315 xxx C***                if        *in35
034200110315 xxx C***                z-add     *zeros        VABNCL
034300110315 xxx C***                z-add     *zeros        VABPKB
034400110315 xxx C***                z-add     *zeros        VABVLB
034500110315 xxx C***                endif
034600080527     C*
034700080527     C* Scrivo il VAB solo a rottura d spedizione
034800080527     C                   if        depspe = *zeros
034900080527     C  N31              eval      fivabds_sav = fivabds
035000080527     C                   eval      depspe = vabNSP
035100080527     C                   else
035200080527     C                   if        depspe = vabNSP AND *in65 = *off
035300120703     C   51              add       VABNCL        sav_VABNCL
035400100413     C   51              add       VABPKB        sav_VABPKB
035500100413     C   51              add       VABVLB        sav_VABVLB
035600100720     C   52              z-add     VABNCL        sav_VABNCL
035700100720     C   52              z-add     VABPKB        sav_VABPKB
035800100720     C   52              z-add     VABVLB        sav_VABVLB
035900100720     C   53              add       VABNCL        sav_VABNCL
036000100720     C   53              z-add     VABPKB        sav_VABPKB
036100100720     C   53              z-add     VABVLB        sav_VABVLB
036200080527     C                   else
036300080527     C  N31              eval      fivabds_ok = fivabds_sav
036400080527     C  N31              exsr      wrivab
036500080527     C  N31              eval      fivabds_sav = fivabds
036600080527     C                   eval      depspe = vabNSP
036700080527     C                   endif
036800080527     C                   endif
036900080527     C*
037000080527     C                   ENDSR
037100040119
037200040119
037300040119
037400040119     C*----------------------------------------------------*
037500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037600040119     C*----------------------------------------------------*
037700040510     C     WRIVAB        BEGSR
037800080513     C*
037900080513     C* Gestisco l'eventuale rottura x numero spedizione
038000080527     C                   if        VABNSP <> depspe or *in65 = *on
038100070301     C                   if        wVATNOT_E <> *blanks
038200070301     C                   eval      VABCTM = '7Q'
038300070301     C                   endif
038400100413     C                   write     fivab000
038500080513     C                   endif
038600040119     C*
038700040119     C                   ENDSR
038800040119
038900040119
039000040119
039100040119     C*----------------------------------------------------*
039200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039300040119     C*----------------------------------------------------*
039400040510     C     WRIVAT        BEGSR
039500060512     C*
039600060512     C* Inizializzo i campi chiave
039700060512     C                   eval      VATFGS = VABFGS
039800060512     C                   eval      VATAAS = VABAAS
039900060512     C                   eval      VATCCM = VABCCM
040000070823     C                   eval      VATNRS = VABNRS
040100060512     C                   eval      VATNSP = VABNSP
040200060512     C                   eval      VATLNP = VABLNP
040300040119     C*
040400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040500040510     C                   if        VATTRC = *blanks
040600040510     C                   eval      VATTRC = 'E'
040700040510     C                   endif
040800110318     C                   if        VATNOT <> *blanks
040900040510     C                   write     FIVAT000
041000040119     C                   add       1             §CTROKVT
041100110318     C                   endif
041200060331     C*
041300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041400060331     C                   if        wVATNOT_A <> *blanks
041500060331     C                   eval      VATNOT = wVATNOT_A
041600060331     C                   eval      VATTRC = 'A'
041700060331     C                   write     FIVAT000
041800060331     C                   add       1             §CTROKVT
041900060331     C                   endif
042000060331     C*
042100060331     C                   if        wVATNOT_B <> *blanks
042200060331     C                   eval      VATNOT = wVATNOT_B
042300060331     C                   eval      VATTRC = 'B'
042400060331     C                   write     FIVAT000
042500060331     C                   add       1             §CTROKVT
042600060331     C                   endif
042700060331     C*
042800060331     C                   if        wVATNOT_E <> *blanks
042900060331     C                   eval      VATTRC = 'E'
043000120629     C*
043100130320     C* NON sviluppo i "CHI SONO" perché spedizione monocollo
043200120703     C*
043300120711     C                   EVAL      VATNOT = wVATNOT_E
043400120629     C                   write     FIVAT000
043500120629     C                   add       1             §CTROKVT
043600120604     C*
043700120604     C                   endif
043800110927     C*
043900110927     C                   if        wVATNOT_I <> *blanks
044000110927     C                   eval      VATNOT = wVATNOT_I
044100110927     C                   eval      VATTRC = 'I'
044200110927     C                   write     FIVAT000
044300110927     C                   add       1             §CTROKVT
044400110927     C                   endif
044500040119     C*
044600040119     C                   ENDSR
044700040119
044800040119
044900040119
045000040119     C*----------------------------------------------------*
045100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045200040119     C*----------------------------------------------------*
045300040119     C     PREVAT        BEGSR
045400040119     C*
045500040119     C* Compongo il nome del membro da dare al FIVATWWR
045600040119     C                   eval      parmbr = vlrhdl
045700040119     C                   movel     'M'           parmbr
045800040510     C                   eval      parccm = vlrksc
045900040119     C                   eval      paropz = '1'
046000040119     C* Effettuo la chiamata al CLLE preposto
046100040119     C                   call(e)   'TITVVTC'
046200040119     C                   parm                    parccm
046300040119     C                   parm                    parmbr
046400040119     C                   parm                    paropz
046500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046600040119     C                   if        %error
046700040119     C                   movel     '1'           chkcall
046800040119     C                   else
046900040119     C                   movel     '0'           chkcall
047000040119     C                   endif
047100040119     C*
047200040119     C                   ENDSR
047300010601
047400010601
047500010601
047600010330     C*----------------------------------------------------*
047700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047800010330     C*----------------------------------------------------*
047900040119     C     INZVAR        BEGSR
048000080527     C*
048100080527     C                   setoff                                       65
048200010330     C*
048300030822     C* Inizializzo variabili di wrk
048400020204     C                   Z-ADD     *zeros        Num5_0            5 0
048500101018     C                   MOVEL     *blanks       wFlgCAS
048600060331     C                   MOVEL     *blanks       wVATNOT_A
048700060331     C                   MOVEL     *blanks       wVATNOT_B
048800060331     C                   MOVEL     *blanks       wVATNOT_E
048900110927     C                   MOVEL     *blanks       wVATNOT_I
049000120411     C                   MOVEL     *blanks       wNOT
049100120411     C                   MOVEL     *blanks       Data8A
049200030822     C*
049300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049400030822     C                   CLEAR                   SkSplitCSV
049500030822     C                   EVAL      i = 1
049600030822     C                   EVAL      posDa = *zeros
049700030822     C                   EVAL      posA  = *zeros
049800080527     C*
049900080527     C                   CLEAR                   FIVABDS
050000080527     C                   CLEAR                   FIVAT000
050100030822     C*
050200030822     C* Reimposto i valori di default
050300030822     C                   EXSR      DEFCAM
050400030822     C*
050500010330     C                   ENDSR
050600010330     C*----------------------------------------------------*
050700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050800010330     C*----------------------------------------------------*
050900010330     C     DEFCAM        BEGSR
051000030822     C*
051100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051200030715     C* e delimitatore testo.
051300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051600100318     C*
051700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051800100318     C                   EVAL      CharSOS = CharNUM
051900030822     C*
052000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052100100720     C                   SETOFF                                       505152
052200100720     C                   SETOFF                                       53
052300100413     C                   SELECT
052400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052500100720     C                   SETON                                        50
052600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052700100413     C                   SETON                                        51
052800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052900100720     C                   SETON                                        52
053000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053100100720     C                   SETON                                        53
053200100413     C                   ENDSL
053300030822     C*
053400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053500030822     C                   EVAL      posDaDft = 1
053600030822     C                   EVAL      posADft  = 0
053700030822     C                   EVAL      wGiroDft = 0
053800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053900030822     C                             posDaDft > 0
054000030822     C*
054100030822     C* Gestisco il 1° giro
054200030822     C                   IF        wGiroDft = 0
054300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054500030822     C* Incremento il contatore dei "giri"
054600030822     C                   EVAL      wGiroDft = 1
054700030822     C                   ELSE
054800030822     C                   EVAL      posDaDft = posADft
054900030822     C                   ENDIF
055000030822     C* Eseguo lo scan x trovare la fine del campo corrente
055100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055200030822     C*
055300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055500030822     C                   IF        posDaDft > 0 AND
055600030822     C                             posADft  > 0
055700030822     C* NCL
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'NCL'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiInt=*on
056700030822     C                   Z-ADD     PiVal         VABNCL
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* CCM
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'CCM'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiInt=*on
058000030822     C                   Z-ADD     PiVal         VABCCM
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* LNP
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'LNP'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiInt=*on
059300030822     C                   Z-ADD     PiVal         VABLNP
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* NRS
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'NRS'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600030822     C                   Z-ADD     PiVal         VABNRS
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* CTR
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'CTR'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiInt=*on
061900030822     C                   Z-ADD     PiVal         VABCTR
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* PKB
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'PKB'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiNum=*on
063200120418     C                   Z-ADD     PiVal         VABPKB
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* VLB
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'VLB'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiNum=*on
064500030822     C                   Z-ADD     PiVal         VABVLB
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* QFT
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'QFT'
065300030822     C                   EVAL      PiStr=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   EXSR      CHKNUM
065700030822     C                   IF        PiNum=*on
065800030822     C                   Z-ADD     PiVal         VABQFT
065900030822     C                   ENDIF
066000030822     C                   ENDIF
066100030822     C* CBO
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'CBO'
066600030822     C                   EVAL      VABCBO=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* TSP
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'TSP'
067500030822     C                   EVAL      VABTSP=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* VAS
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'VAS'
068400030822     C                   EVAL      VABVAS=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* VCA
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'VCA'
069300030822     C                   EVAL      VABVCA=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700070925     C* TIC
069800070925     C                   IF        %subst(
069900070925     C                             %subst(vlrppt:posDaDft+1:
070000070925     C                             posADft-posDaDft-1):1:3)
070100070925     C                             = 'TIC'
070200070925     C                   EVAL      VABTIC=%trim(%subst(
070300070925     C                             %subst(vlrppt:posDaDft+1:
070400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500070925     C                   ENDIF
070600030822     C* GCA
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'GCA'
071100030822     C                   EVAL      VABGCA=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* CTM
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'CTM'
072000030822     C                   EVAL      VABCTM=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* FFD
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'FFD'
072900030822     C                   EVAL      VABFFD=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* VAD
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'VAD'
073800030822     C                   EVAL      VABVAD=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* GMA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GMA'
074700030822     C                   EVAL      VABGMA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* GGA
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'GGA'
075600030822     C                   EVAL      VABGGA=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* GVA
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'GVA'
076500030822     C                   EVAL      VABGVA=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* TC1
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'TC1'
077400030822     C                   EVAL      VABTC1=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* TC2
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'TC2'
078300030822     C                   EVAL      VABTC2=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700040714     C* VATTRC
078800040714     C                   IF        %subst(
078900040714     C                             %subst(vlrppt:posDaDft+1:
079000040714     C                             posADft-posDaDft-1):1:3)
079100040714     C                             = 'TRC'
079200040714     C                   EVAL      VATTRC=%trim(%subst(
079300040714     C                             %subst(vlrppt:posDaDft+1:
079400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500040714     C                   ENDIF
079600030822     C* ...
079700030822     C                   ENDIF
079800030822     C                   ENDDO
079900020204     C*
080000010330     C                   ENDSR
080100010607     C*----------------------------------------------------*
080200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080300010607     C*----------------------------------------------------*
080400010607     C     IMPVAB        BEGSR
080500010607     C*
080600010607     C                   SETOFF                                       3132
080700110315 xxx C***                SETON                                        35
080800030515     C*
080900010607     C                   MOVEL     datcor        VABAAS
081000010607     C                   MOVE      datcor        VABMGS
081100010607     C*
081200030715     C**********
081300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081400030715     C**********
081500030715     C                   IF        wGiro = *zeros
081600040113     C*
081700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
082000080429     C                             %trim(intcol(5))<>*blanks
082100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082300080429     C                                      %trim(intcol(5))
082400070823     C     ';':CharCSV   XLATE     vindta        vindta
082500040510     C                   ENDIF
082600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082700031201     C     minu:maiu     XLATE     vindta        vindta
082800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082900030715     C                   DOW       posDa <= %len(%trim(vindta))
083000030715     C*
083100030715     C* Gestisco il 1° campo
083200030715     C                   IF        i = 1
083300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083400030715     C                   EVAL      posDa = 1
083500030715     C* Eseguo lo scan x trovare la fine del primo campo
083600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C* X i campi successivi al 1°
084100030715     C                   ELSE
084200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084300030715     C                   EVAL      posDa = posA + 1
084400030715     C* Eseguo lo scan x trovare la fine del campo corrente
084500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084600030715     C* Gestisco l'ultimo campo
084700030715     C                   IF        posA = *zeros
084800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C                   LEAVE
085300030715     C                   ELSE
085400030715     C* A questo punto "estraggo" il campo corrente
085500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085600030715     C                                                    (posA-posDa))
085700030715     C                   ENDIF
085800030715     C                   ENDIF
085900030715     C* Incremento il contatore d campo
086000030715     C                   EVAL      i = i +1
086100030715     C                   ENDDO
086200030715     C*
086300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086400030715     C* delimitazione testo
086500030715     C                   EVAL      i = 1
086600030715     C                   DOW       i < %elem(SkSplitFLD)
086700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086900030715     C                   EVAL      i = i + 1
087000030715     C                   ENDDO
087100030715     C                   EVAL      wGiro = 1
087200030715     C                   ELSE
087300030715     C                   EVAL      wGiro = 2
087400031204     C**********
087500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087600031204     C* d separatore campo
087700031204     C**********
087800031204     C                   EVAL      vindta = %trim(vindta)
087900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088000031204     C                   EVAL      vindta = %subst(vindta:2)
088100031204     C                   ENDDO
088200031204     C*
088300031204     C                   Z-ADD     *zeros        lunghInput        4 0
088400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088800110912     C                   ENDDO
088900030715     C**********
089000030715     C* Effettuo lo split del campo dati d input
089100030715     C**********
089200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089300030715     C                   DOW       posDa <= %len(%trim(vindta))
089400030715     C*
089500030715     C* Gestisco il 1° campo
089600030715     C                   IF        i = 1
089700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089800030715     C                   EVAL      posDa = 1
089900030715     C* Eseguo lo scan x trovare la fine del primo campo
090000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090100030715     C* A questo punto "estraggo" il campo corrente
090200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090300030715     C                                                    (posA-posDa))
090400030715     C* X i campi successivi al 1°
090500030715     C                   ELSE
090600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090700030715     C                   EVAL      posDa = posA + 1
090800031204     C* Verifico che nn vi sia il campo nn valorizzato
090900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091000031204     C* Se campo nn valorizzato skippo al prossimo
091100031204     C                   EVAL      SkSplitCSV(i) = *blanks
091200031204     C                   EVAL      posA  = posA + 1
091300031204     C                   ELSE
091400030715     C* Eseguo lo scan x trovare la fine del campo corrente
091500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091600030715     C* Gestisco l'ultimo campo
091700030715     C                   IF        posA = *zeros
091800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C                   LEAVE
092300030715     C                   ELSE
092400030715     C* A questo punto "estraggo" il campo corrente
092500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092600030715     C                                                    (posA-posDa))
092700030715     C                   ENDIF
092800031204     C                   ENDIF
092900030715     C                   ENDIF
093000030715     C* Incremento il contatore d campo
093100030715     C                   EVAL      i = i +1
093200030715     C                   ENDDO
093300030715     C*
093400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093500030715     C* delimitazione testo
093600030715     C                   EVAL      i = 1
093700030715     C                   DOW       i < %elem(SkSplitCSV)
093800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094000030715     C                   EVAL      i = i + 1
094100030715     C                   ENDDO
094200100720     C*
094300130222     C* Stacco un progressivo x numero spedizione
094400130222     C                   EXSR      repNSP
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
096300030715     C                   ENDIF
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
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112200030721     C                   EVAL      PiStr=SkSplitCSV(i)
112300030721     C                   EXSR      CHKNUM
112400030721     C                   IF        PiInt=*on
112500030721     C                   Z-ADD     PiVal         Num5_0
112600030721     C                   MOVEL(p)  Num5_0        VABCAD
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABCAD = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100030721     C                             + ' ' + 'VABCAD'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** NCL
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on
113900030721     C                   Z-ADD     PiVal         VABNCL
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200030721     C                   EVAL      VABNCL = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400040122     C                             + ' ' + 'VABNCL'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** PKB
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiNum=*on
115200120418     C                   Z-ADD(H)  PiVal         VABPKB
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABPKB = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABPKB'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** CAS
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116300111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
116400120713     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
116500120713     C                             %trim(SkSplitCSV(i)) <> '0,00'
116600120605     C                   EVAL      VABVCA = 'EUR'
116700101018     C                   EVAL      wFlgCAS = '1'
116800101018     C                   ENDIF
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiNum=*on
117200030721     C                   Z-ADD     PiVal         VABCAS
117300030721     C                   ELSE
117400030721     C                   SETON                                        32
117500030721     C                   EVAL      VABCAS = *zeros
117600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117700030721     C                             + ' ' + 'VABCAS'
117800030721     C                   ENDIF
117900030721     C                   ENDIF
118000030721     C*** CCM
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiInt=*on      AND
118500030721     C                             PiVal<=9999999 AND
118600030721     C                             PiVal>*zeros
118700030721     C                   Z-ADD     PiVal         VABCCM
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      VABCCM = *zeros
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200040510     C                             + ' ' + 'VABCCM'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** LNP
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiInt=*on    AND
120000030721     C                             PiVal<=999   AND
120100030721     C                             PiVal>*zeros
120200030721     C                   Z-ADD     PiVal         VABLNP
120300030721     C                   ELSE
120400040715     C                   SETON                                        32
120500030721     C                   EVAL      VABLNP = *zeros
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700040510     C                             + ' ' + 'VABLNP'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** NRS
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on AND
121500030721     C                             PiVal<=99
121600030721     C                   Z-ADD     PiVal         VABNRS
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABNRS'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** LNA
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on    AND
122800030721     C                             PiVal<=999   AND
122900030721     C                             PiVal>*zeros
123000030721     C                   Z-ADD     PiVal         VABLNA
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      VABLNA = *zeros
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABLNA'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** CTR
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on  AND
124300030721     C                             PiVal<=999
124400030721     C                   Z-ADD     PiVal         VABCTR
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABCTR'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** IAS
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiNum=*on
125600030721     C                   Z-ADD     PiVal         VABIAS
125700130222     C                   EVAL      vabVAS = 'EUR'
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
141900120713 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
142000130322
142100130322     C*** W_VABRMN contiene sia RMN che RMA
142200130322     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142300130322
142400130322     C                   EVAL      VABRMA = SkSplitCSV(i)
142500130322
142600130322     C                   EVAL      PiStr=SkSplitCSV(i)
142700130322     C                   EXSR      CHKNUM
142800130322     C                   IF        PiInt=*on
142900130322     C                   Z-ADD     PiVal         VABRMN
143000130322     C                   ELSE
143100130322     C                   SETON                                        32
143200130322     C                   EVAL      VABRMN = *zeros
143300130322     C                   EVAL      vinmsg = %trimr(vinmsg)
143400130322     C                             + ' ' + 'VABRMN'
143500130322     C                   ENDIF
143600130322
143700130322     C                   ENDIF
143800120713
143900130402     C*** W_VABCBO
144000130402     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
144100130322     C* non c'è bisogno dell'xlate avendola già fatta su tutto VINDTA
144200130402     C                   IF        SkSplitCSV(i)='FRANCO'
144300130402     C                   EVAL      VABCBO = '1'
144400130402     C                   ELSE
144500130402     C                   EVAL      VABCBO = '2'
144600130402     C                   ENDIF
144700130322     C                   ENDIF
144800120301
144900070925     C*
145000071029     C***  <===  -----------------------------------------
145100040510     C*
145200030715     C                   ENDIF
145300030715     C                   EVAL      i = i + 1
145400030715     C                   ENDDO
145500070925     C*
145600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145700120411     C*
145800070925     C*
145900071029     C***  <===  -----------------------------------------
146000070925     C*
146100070925     C* Considerazioni finali su CBO/CAS
146200100318     C                   IF        wFlgCAS = '1'    OR
146300100318     C                             VABCAS  > *zeros
146400070925     C                   IF        VABCBO = '1'
146500070925     C                   EVAL      VABCBO = '4'
146600070925     C                   ENDIF
146700070925     C                   IF        VABCBO = '2'
146800070925     C                   EVAL      VABCBO = '6'
146900070925     C                   ENDIF
147000130402     C* se valorizzato il c/assegno, il  tipo incasso è TM
147100130402     C                   EVAL      VABTIC = 'TM'
147200070925     C                   ENDIF
147300070925     C***  <===  ----------------------------
147400040510     C*
147500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147600040510     C                   IF        VABNSP = *zeros
147700040510     C                   SETON                                        31
147800040510     C                   ENDIF
147900040715     C*
148000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148100040715     C                   IF        VABLNP = *zeros
148200040715     C                   SETON                                        31
148300040715     C                   ENDIF
148400040510     C*
148500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148600120411     C                   IF        VABRMN = *zeros
148700120411     C                   Z-ADD     VABNSP        VABRMN
148800120411     C                   ENDIF
148900120328     C* IN QUESTO CASO LO FORZO = 1
149000120411     C***                IF        VABRMN = *zeros
149100120411     C***                Z-ADD     1             VABRMN
149200120411     C***                ENDIF
149300040510     C*
149400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149500040510     C                   IF        VABRMA = *blanks
149600040714     C                   MOVEL     VABRMN        VABRMA
149700040510     C                   ENDIF
149800040128     C*
149900040510     C* Se provincia nn valorizzata la reperisco
150000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150100110912     C                   IF        VABPRD =  *blanks AND
150200110912     C                             VABNZD =  *blanks AND
150300110912     C                             VABCAD <> *blanks AND
150400110912     C                             VABLOD <> *blanks
150500040128     C                   CLEAR                   TISI95DS
150600040128     C                   EVAL      I95TCN = '3'
150700040128     C                   Z-ADD     datcor        I95DAT
150800040128     C                   EVAL      I95NAR = VABNZD
150900040128     C                   EVAL      I95CAP = VABCAD
151000040128     C                   EVAL      I95LOC = VABLOD
151100040128     C                   CALL      'TISI95R'
151200040128     C                   PARM                    TISI95DS
151300040128     C                   EVAL      VABPRD = O95PRV
151400040510     C                   ENDIF
151500020204     C*
151600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151700020204     C                   EXSR      CHKIMPDIV
151800030715     C                   ENDIF
151900020204     C*
152000010607     C                   ENDSR
152100010607     C*----------------------------------------------------*
152200010601
152300020204
152400020204     C*----------------------------------------------------*
152500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152600020204     C*----------------------------------------------------*
152700020204     C     CHKIMPDIV     BEGSR
152800020204     C*
152900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153000020204     C                   Z-ADD     *zeros        wrkDec            9 9
153100020204     C*
153200020204     C* Come prima cosa effettuo considerazioni sulla divisa
153300020204     C                   IF        vabIAS > *zeros
153400020204     C                   IF        vabVAS <> 'EUR'
153500020204     C                   EVAL      vabVAS =  'ITL'
153600020204     C                   ENDIF
153700020204     C                   ENDIF
153800020204     C*
153900020204     C                   IF        vabCAS > *zeros
154000020204     C                   IF        vabVCA <> 'EUR'
154100020204     C                   EVAL      vabVCA =  'ITL'
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C                   IF        vabVMD > *zeros
154600020204     C                   IF        vabVAD <> 'EUR'
154700020204     C                   EVAL      vabVAD =  'ITL'
154800020204     C                   ENDIF
154900020204     C                   ENDIF
155000020204     C*
155100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155200020204     C                   Z-ADD     vabIAS        wrkDec
155300020204     C                   IF        wrkDec > *zeros
155400020204     C                   IF        vabVAS = 'ITL'
155500020204     C                   EVAL      vabIAS = *zeros
155600020204     C                   ENDIF
155700020204     C                   ENDIF
155800020204     C*
155900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156000020204     C                   Z-ADD     vabCAS        wrkDec
156100020204     C                   IF        wrkDec > *zeros
156200020204     C                   IF        vabVCA = 'ITL'
156300020204     C                   EVAL      vabCAS = *zeros
156400020204     C                   ENDIF
156500020204     C                   ENDIF
156600020204     C*
156700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156800020204     C                   Z-ADD     vabVMD        wrkDec
156900020204     C                   IF        wrkDec > *zeros
157000020204     C                   IF        vabVAD = 'ITL'
157100020204     C                   EVAL      vabVMD = *zeros
157200020204     C                   ENDIF
157300020204     C                   ENDIF
157400020204     C*
157500020204     C                   ENDSR
157600020204     C***
157700010330
157800100318
157900100318
158000100318     C*----------------------------------------------------*
158100100318     C*  CONTROLLO NUMERICITA' CAMPI
158200100318     C*----------------------------------------------------*
158300100318     C     CHKNUM        BEGSR
158400100318     C*
158500100318     C                   IF        PiDecChr = *blanks
158600100318     C                   EVAL      PiDecChr = CharNUM
158700100318     C                   ENDIF
158800100318     C*
158900100318     C                   callp     UBISNUM_Check(PiStr
159000100318     C                                          :PiDecChr
159100100318     C                                          :PiVal
159200100318     C                                          :PiNum
159300100318     C                                          :PiInt)
159400100318     C*
159500100318     C                   ENDSR
159600100318     C***
159700010601
159800010601
159900010601
160000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160100010601     C     repfil        BEGSR
160200010601     C*
160300010601     C                   if        invfil = *zeros and
160400010601     C                             depfil > *zeros and
160500010629     C                             (vinflg = *blanks or
160600010629     C                              vinflg = *zeros)
160700010601     C
160800010601     C                   eval      invfil = depfil
160900010601     C                   endif
161000010601     C*
161100010601     C                   if        depfil <> invfil and
161200010601     C                             invfil > *zeros
161300010601     C                   eval      flgMulti = '1'
161400010601     C                   if        vinflg = *blanks
161500010601     C                   add       1             cntNonEl
161600010601     C                   endif
161700010601     C                   endif
161800010601     C*
161900010601     C                   if        vinflg = '2'
162000010601     C                   eval      flgStato = '2'
162100010601     C                   endif
162200010601     C*
162300010601     C                   ENDSR
162400010601     C***
162500010601
162600010601
162700010601
162800010330
162900040119      /TITLE Invio dei dati al punto operativo.
163000040119     C     invio         BEGSR
163100040119     C*
163200040119     C* 1° invio FIVAT
163300040119     C                   reset                   dscmz
163400040119     C                   move      vlrpoi        cmzdst
163500040119     C                   eval      cmzfld = 'FIVATWWR'
163600040119     C                   eval      cmzmbd = vlrhdl
163700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163800040119     C***                if        prmfir = *blanks
163900040119     C                   eval      cmzfla = 'FIVAT00F'
164000040119     C                   eval      cmzmba = 'FIVAT00F'
164100040119     C***                else
164200040119     C***                eval      cmzfla = prmfir
164300040119     C***                eval      cmzmba = prmfir
164400040119     C***                endif
164500040119     C                   eval      cmznrr = *zeros
164600040119     C                   move      §ctrokvt      cmznrr
164700040119     C                   eval      cmzlba = vlrfl1
164800040119     C                   call(e)   'TIS711C'
164900040119     C                   parm                    dscmz
165000040119     C                   parm      *blanks       esito
165100040119     C                   if        %error
165200040119     C                             or cmzerr = '1'
165300040119     C                             or esito  = '1'
165400040119     C                   eval      wrkesito = '3'
165500040119     C                   else
165600040119     C*
165700040119     C* 2° invio FIVAB
165800040119     C                   reset                   dscmz
165900040119     C                   move      vlrpoi        cmzdst
166000040119     C                   eval      cmzfld = vlrfou
166100040119     C                   eval      cmzmbd = vlrhdl
166200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166300040119     C***                if        prmfir = *blanks
166400040119     C                   eval      cmzfla = 'FIVAB00F'
166500040119     C                   eval      cmzmba = 'FIVAB00F'
166600040119     C***                else
166700040119     C***                eval      cmzfla = prmfir
166800040119     C***                eval      cmzmba = prmfir
166900040119     C***                endif
167000040119     C                   eval      cmznrr = *zeros
167100040119     C                   move      §ctrokvb      cmznrr
167200040119     C                   eval      cmzlba = vlrfl1
167300040119     C                   call(e)   'TIS711C'
167400040119     C                   parm                    dscmz
167500040119     C                   parm      *blanks       esito
167600040119     C                   if        %error
167700040119     C                             or cmzerr = '1'
167800040119     C                             or esito  = '1'
167900040119     C                   eval      wrkesito = '3'
168000040119     C                   endif
168100040119     C                   endif
168200040119     C*
168300040119     C                   ENDSR
168400040119     C***
168500130328
168600130328     C*--------------------------------------------------------
168700130328     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
168800130328     C*--------------------------------------------------------
168900130328     C     CARTAB        BEGSR
169000130328     C*
169100130328     C* TABELLA '15' - NAZIONI
169200130328     C                   clear                   skNAZISO
169300130328     C                   clear                   skNAZBAR
169400130328     C                   eval      tblKUT = 1
169500130328     C                   eval      tblCOD = '15'
169600130328     C     KEYtabP       setll     tabel00f
169700130328     C     KEYtabP       reade     tabel00f
169800130328     C                   dow       not %eof(tabel00f)
169900130328     C                   if        tblFLG = *blanks
170000130328     C                   movel(p)  tblUNI        ds15
170100130328     C                   add       1             jNAZ
170200130328     C                   eval      skNAZBAR(jNAZ) = tblKEY
170300130328     C                   eval      skNAZISO(jNAZ) = §15COD
170400130328     C                   endif
170500130328     C     KEYtabP       reade     tabel00f
170600130328     C                   enddo
170700130328     C*
170800130328     C                   ENDSR
170900130328     C***
171000010601
171100010601      /TITLE Invio dei dati al punto operativo.
171200010601     C     opeini        BEGSR
171300010601     C*
171400010601     C* Inizializzo flag e contatori operativi
171500010601     C                   movel     '0'           flgGiro           1
171600010601     C                   movel     '0'           flgMulti          1
171700010601     C                   movel     '1'           flgStato          1
171800010615     C                   movel     '0'           flgOk             1
171900010601     C                   z-add     *zeros        cntNonEl         10 0
172000010601     C                   z-add     *zeros        depfil            3 0
172100010601     C                   z-add     *zeros        invfil            3 0
172200010601     C*
172300010601     C                   ENDSR
172400010601     C***
172500070326
172600070326
172700070326
172800070326
172900070326     C     *pssr         BEGSR
173000070329     C*
173100070329     C                   if        %open(tivin00r)
173200070329     C                   close     tivin00r
173300070329     C                   endif
173400070329     C                   if        %open(fivabwwr)
173500070329     C                   close     fivabwwr
173600070329     C                   endif
173700070329     C                   if        %open(fivatwwr)
173800070329     C                   close     fivatwwr
173900070329     C                   endif
174000070326     C*
174100070326     C* Effettuo la chiamata al CLLE preposto
174200070326     C                   call(e)   'TITVVTC'
174300070326     C                   parm                    parccm
174400070326     C                   parm                    parmbr
174500070326     C                   parm      '2'           paropz
174600070326     C*
174700070326     C                   eval      wrkesito = '2'
174800070404     C*
174900070404     C                   seton                                        LR
175000070326     C*
175100070326     C                   ENDSR     '*CANCL'
175200070326     C***
175300100318
175400100318
175500100318
175600100318     C     normCSV       BEGSR
175700100318     C*
175800100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175900100318     C* separatori campi
176000100318     C                   if        CharTXT <> *blanks
176100100318     C                   z-add     *zeros        wLenVINDTA        4 0
176200100318     C                   z-add     *zeros        wFlgVINDTA        1 0
176300100318     C                   setoff                                       80
176400100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
176500100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
176600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176700100318     C                   if        not *in80
176800100318     C                   seton                                        80
176900100318     C                   else
177000100318     C                   setoff                                       80
177100100318     C                   endif
177200100318     C                   endif
177300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
177400100318     C                             and *in80 = *on
177500100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
177600100318     C                   endif
177700100318     C                   enddo
177800100318     C                   endif
177900100318     C*
178000100318     C                   ENDSR
178100100318     C***
178200070326
178300100326
178400100326
178500100326     C*----------------------------------------------------*
178600100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178700100326     C*----------------------------------------------------*
178800100326     C     REPNSP        BEGSR
178900100326     C*
179000100326     C* NSP => Stacco un numeratore da AZNUM
179100100326     C                   clear                   TRUL33DS
179200100326     C                   eval      I33OPE = *zeros
179300100326     C                   eval      I33CNU = 302
179400100326     C                   eval      I33NUM = 1
179500100326     C                   movel     TRUL33DS      KPJBU
179600100326     C                   call      'TRUL33R'
179700100326     C                   parm                    KPJBA
179800100326     C                   movel     KPJBU         TRUL33DS
179900100326     C                   if        O33ERR = *zeros
180000100326     C                   z-add     O33NRF        VABNSP
180100100326     C                   z-add     O33NRF        VATNSP
180200100326     C                   else
180300100326     C                   SETON                                        31
180400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
180500100326     C                             + ' ' + 'VABNSP VATNSP'
180600100326     C                   endif
180700100326     C*
180800100326     C                   ENDSR
180900010330
181000100326
181100010330
181200000613     C     *inzsr        BEGSR
181300990910     C*
181400990910     C     *entry        plist
181500990920     C                   parm                    tivlrds
181600990921     C                   parm      wrkesito      esito
181700000724     C                   parm                    prmlit
181800000710     C                   parm                    prmfir
181900010330     C*
182000010330     C* CALCOLA LA DATA CORRENTE
182100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182200100318     C                   eval      datcor = %dec(%date() : *ISO)
182300130328     C*
182400130328     C* Chiave su TABEL00F - parziale
182500130328     C     KEYtabP       KLIST
182600130328     C                   KFLD                    tblKUT
182700130328     C                   KFLD                    tblCOD
182800000613     C*
182900000613     C                   ENDSR
183000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
183100130402W_VABRMN;W_VABCBO;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;VABCAS;W_VABNOT;
183200130402
183300120604
183400130322
183500120604
