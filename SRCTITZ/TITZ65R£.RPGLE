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
036700070301     C                   eval      VABCTM = '7Q'
036800070301     C                   endif
036900100413     C                   write     fivab000
037000080513     C                   endif
037100040119     C*
037200040119     C                   ENDSR
037300040119
037400040119
037500040119
037600040119     C*----------------------------------------------------*
037700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037800040119     C*----------------------------------------------------*
037900040510     C     WRIVAT        BEGSR
038000060512     C*
038100060512     C* Inizializzo i campi chiave
038200060512     C                   eval      VATFGS = VABFGS
038300060512     C                   eval      VATAAS = VABAAS
038400060512     C                   eval      VATCCM = VABCCM
038500070823     C                   eval      VATNRS = VABNRS
038600060512     C                   eval      VATNSP = VABNSP
038700060512     C                   eval      VATLNP = VABLNP
038800040119     C*
038900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039000040510     C                   if        VATTRC = *blanks
039100040510     C                   eval      VATTRC = 'E'
039200040510     C                   endif
039300110318     C                   if        VATNOT <> *blanks
039400040510     C                   write     FIVAT000
039500040119     C                   add       1             §CTROKVT
039600110318     C                   endif
039700060331     C*
039800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039900060331     C                   if        wVATNOT_A <> *blanks
040000060331     C                   eval      VATNOT = wVATNOT_A
040100060331     C                   eval      VATTRC = 'A'
040200060331     C                   write     FIVAT000
040300060331     C                   add       1             §CTROKVT
040400060331     C                   endif
040500060331     C*
040600060331     C                   if        wVATNOT_B <> *blanks
040700060331     C                   eval      VATNOT = wVATNOT_B
040800060331     C                   eval      VATTRC = 'B'
040900060331     C                   write     FIVAT000
041000060331     C                   add       1             §CTROKVT
041100060331     C                   endif
041200060331     C*
041300060331     C                   if        wVATNOT_E <> *blanks
041400060331     C                   eval      VATTRC = 'E'
041500120629     C*
041600120711     C* NON sviluppo i "CHI SONO" perché sono indicati sul csv 1 riga x collo
041700120703     C*
041800120711     C                   EVAL      VATNOT = wVATNOT_E
041900120629     C                   write     FIVAT000
042000120629     C                   add       1             §CTROKVT
042100120604     C*
042200120604     C                   endif
042300110927     C*
042400110927     C                   if        wVATNOT_I <> *blanks
042500110927     C                   eval      VATNOT = wVATNOT_I
042600110927     C                   eval      VATTRC = 'I'
042700110927     C                   write     FIVAT000
042800110927     C                   add       1             §CTROKVT
042900110927     C                   endif
043000040119     C*
043100040119     C                   ENDSR
043200040119
043300040119
043400040119
043500040119     C*----------------------------------------------------*
043600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043700040119     C*----------------------------------------------------*
043800040119     C     PREVAT        BEGSR
043900040119     C*
044000040119     C* Compongo il nome del membro da dare al FIVATWWR
044100040119     C                   eval      parmbr = vlrhdl
044200040119     C                   movel     'M'           parmbr
044300040510     C                   eval      parccm = vlrksc
044400040119     C                   eval      paropz = '1'
044500040119     C* Effettuo la chiamata al CLLE preposto
044600040119     C                   call(e)   'TITVVTC'
044700040119     C                   parm                    parccm
044800040119     C                   parm                    parmbr
044900040119     C                   parm                    paropz
045000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045100040119     C                   if        %error
045200040119     C                   movel     '1'           chkcall
045300040119     C                   else
045400040119     C                   movel     '0'           chkcall
045500040119     C                   endif
045600040119     C*
045700040119     C                   ENDSR
045800010601
045900010601
046000010601
046100010330     C*----------------------------------------------------*
046200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046300010330     C*----------------------------------------------------*
046400040119     C     INZVAR        BEGSR
046500080527     C*
046600080527     C                   setoff                                       65
046700010330     C*
046800030822     C* Inizializzo variabili di wrk
046900020204     C                   Z-ADD     *zeros        Num5_0            5 0
047000101018     C                   MOVEL     *blanks       wFlgCAS
047100060331     C                   MOVEL     *blanks       wVATNOT_A
047200060331     C                   MOVEL     *blanks       wVATNOT_B
047300060331     C                   MOVEL     *blanks       wVATNOT_E
047400110927     C                   MOVEL     *blanks       wVATNOT_I
047500120411     C                   MOVEL     *blanks       wNOT
047600120411     C                   MOVEL     *blanks       Data8A
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
061700120418     C                   Z-ADD     PiVal         VABPKB
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
092800120711     C* NON Stacco un progressivo x numero spedizione perché il dato è nel csv
092900120711 xxx C***                   EXSR      repNSP
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
113300120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
113400120418     C* per cui si arrotonda il valore passato agli etti
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiNum=*on
113900120418     C                   Z-ADD(H)  PiVal         VABPKB
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200030721     C                   EVAL      VABPKB = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABPKB'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** CAS
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114900110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115000111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115100120713     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
115200120713     C                             %trim(SkSplitCSV(i)) <> '0,00'
115300120605     C                   EVAL      VABVCA = 'EUR'
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
140500120713 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
140600120713
140700120711     C*** W_VABNOT può contenere sia VABNOT che VABNT2
140800120711     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
140900120711     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
141000120711     C                   ENDIF
141100120717     C*** NSP / RMN / RMA
141200120713     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
141300120713     C                   EVAL      PiStr=SkSplitCSV(i)
141400120713     C                   EXSR      CHKNUM
141500120713     C                   IF        PiInt=*on
141600120713     C                   Z-ADD     PiVal         VABNSP
141700120713     C                   Z-ADD     PiVal         VABRMN
141800120717     C                   MOVEL     datcor        VABRMA
141900120717     C                   EVAL      %subst(VABRMA:5:5) = %subst(
142000120717     C                              %editc(VABRMN:'X') : 11:5)
142100120713     C                   ELSE
142200120713     C                   SETON                                        31
142300120713     C                   EVAL      VABNSP = *zeros
142400120713     C                   EVAL      VABRMN = 1
142500120713     C                   EVAL      vinmsg = %trimr(vinmsg)
142600120717     C                             + ' ' + 'VABNSP VABRMN VABRMA'
142700120713     C                   ENDIF
142800120713     C                   ENDIF
142900120301
143000070925     C*
143100071029     C***  <===  -----------------------------------------
143200040510     C*
143300030715     C                   ENDIF
143400030715     C                   EVAL      i = i + 1
143500030715     C                   ENDDO
143600070925     C*
143700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
143800120411     C*
143900120711     C*** W_VABNOT può contenere sia VABNOT che VABNT2
144000120711     C                   EVAL      VABNOT = %subst(wNOT:1:35)
144100120711     C                   EVAL      VABNT2 = %subst(wNOT:36:35)
144200070925     C*
144300071029     C***  <===  -----------------------------------------
144400070925     C*
144500070925     C* Considerazioni finali su CBO/CAS
144600100318     C                   IF        wFlgCAS = '1'    OR
144700100318     C                             VABCAS  > *zeros
144800070925     C                   IF        VABCBO = '1'
144900070925     C                   EVAL      VABCBO = '4'
145000070925     C                   ENDIF
145100070925     C                   IF        VABCBO = '2'
145200070925     C                   EVAL      VABCBO = '6'
145300070925     C                   ENDIF
145400070925     C                   ENDIF
145500070925     C***  <===  ----------------------------
145600040510     C*
145700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145800040510     C                   IF        VABNSP = *zeros
145900040510     C                   SETON                                        31
146000040510     C                   ENDIF
146100040715     C*
146200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146300040715     C                   IF        VABLNP = *zeros
146400040715     C                   SETON                                        31
146500040715     C                   ENDIF
146600040510     C*
146700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146800120411     C                   IF        VABRMN = *zeros
146900120411     C                   Z-ADD     VABNSP        VABRMN
147000120411     C                   ENDIF
147100120328     C* IN QUESTO CASO LO FORZO = 1
147200120411     C***                IF        VABRMN = *zeros
147300120411     C***                Z-ADD     1             VABRMN
147400120411     C***                ENDIF
147500040510     C*
147600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147700040510     C                   IF        VABRMA = *blanks
147800040714     C                   MOVEL     VABRMN        VABRMA
147900040510     C                   ENDIF
148000040128     C*
148100040510     C* Se provincia nn valorizzata la reperisco
148200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148300110912     C                   IF        VABPRD =  *blanks AND
148400110912     C                             VABNZD =  *blanks AND
148500110912     C                             VABCAD <> *blanks AND
148600110912     C                             VABLOD <> *blanks
148700040128     C                   CLEAR                   TISI95DS
148800040128     C                   EVAL      I95TCN = '3'
148900040128     C                   Z-ADD     datcor        I95DAT
149000040128     C                   EVAL      I95NAR = VABNZD
149100040128     C                   EVAL      I95CAP = VABCAD
149200040128     C                   EVAL      I95LOC = VABLOD
149300040128     C                   CALL      'TISI95R'
149400040128     C                   PARM                    TISI95DS
149500040128     C                   EVAL      VABPRD = O95PRV
149600040510     C                   ENDIF
149700020204     C*
149800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149900020204     C                   EXSR      CHKIMPDIV
150000030715     C                   ENDIF
150100020204     C*
150200010607     C                   ENDSR
150300010607     C*----------------------------------------------------*
150400010601
150500020204
150600020204     C*----------------------------------------------------*
150700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150800020204     C*----------------------------------------------------*
150900020204     C     CHKIMPDIV     BEGSR
151000020204     C*
151100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
151200020204     C                   Z-ADD     *zeros        wrkDec            9 9
151300020204     C*
151400020204     C* Come prima cosa effettuo considerazioni sulla divisa
151500020204     C                   IF        vabIAS > *zeros
151600020204     C                   IF        vabVAS <> 'EUR'
151700020204     C                   EVAL      vabVAS =  'ITL'
151800020204     C                   ENDIF
151900020204     C                   ENDIF
152000020204     C*
152100020204     C                   IF        vabCAS > *zeros
152200020204     C                   IF        vabVCA <> 'EUR'
152300020204     C                   EVAL      vabVCA =  'ITL'
152400020204     C                   ENDIF
152500020204     C                   ENDIF
152600020204     C*
152700020204     C                   IF        vabVMD > *zeros
152800020204     C                   IF        vabVAD <> 'EUR'
152900020204     C                   EVAL      vabVAD =  'ITL'
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153400020204     C                   Z-ADD     vabIAS        wrkDec
153500020204     C                   IF        wrkDec > *zeros
153600020204     C                   IF        vabVAS = 'ITL'
153700020204     C                   EVAL      vabIAS = *zeros
153800020204     C                   ENDIF
153900020204     C                   ENDIF
154000020204     C*
154100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
154200020204     C                   Z-ADD     vabCAS        wrkDec
154300020204     C                   IF        wrkDec > *zeros
154400020204     C                   IF        vabVCA = 'ITL'
154500020204     C                   EVAL      vabCAS = *zeros
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
155000020204     C                   Z-ADD     vabVMD        wrkDec
155100020204     C                   IF        wrkDec > *zeros
155200020204     C                   IF        vabVAD = 'ITL'
155300020204     C                   EVAL      vabVMD = *zeros
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C                   ENDSR
155800020204     C***
155900010330
156000100318
156100100318
156200100318     C*----------------------------------------------------*
156300100318     C*  CONTROLLO NUMERICITA' CAMPI
156400100318     C*----------------------------------------------------*
156500100318     C     CHKNUM        BEGSR
156600100318     C*
156700100318     C                   IF        PiDecChr = *blanks
156800100318     C                   EVAL      PiDecChr = CharNUM
156900100318     C                   ENDIF
157000100318     C*
157100100318     C                   callp     UBISNUM_Check(PiStr
157200100318     C                                          :PiDecChr
157300100318     C                                          :PiVal
157400100318     C                                          :PiNum
157500100318     C                                          :PiInt)
157600100318     C*
157700100318     C                   ENDSR
157800100318     C***
157900010601
158000010601
158100010601
158200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158300010601     C     repfil        BEGSR
158400010601     C*
158500010601     C                   if        invfil = *zeros and
158600010601     C                             depfil > *zeros and
158700010629     C                             (vinflg = *blanks or
158800010629     C                              vinflg = *zeros)
158900010601     C
159000010601     C                   eval      invfil = depfil
159100010601     C                   endif
159200010601     C*
159300010601     C                   if        depfil <> invfil and
159400010601     C                             invfil > *zeros
159500010601     C                   eval      flgMulti = '1'
159600010601     C                   if        vinflg = *blanks
159700010601     C                   add       1             cntNonEl
159800010601     C                   endif
159900010601     C                   endif
160000010601     C*
160100010601     C                   if        vinflg = '2'
160200010601     C                   eval      flgStato = '2'
160300010601     C                   endif
160400010601     C*
160500010601     C                   ENDSR
160600010601     C***
160700010601
160800010601
160900010601
161000010330
161100040119      /TITLE Invio dei dati al punto operativo.
161200040119     C     invio         BEGSR
161300040119     C*
161400040119     C* 1° invio FIVAT
161500040119     C                   reset                   dscmz
161600040119     C                   move      vlrpoi        cmzdst
161700040119     C                   eval      cmzfld = 'FIVATWWR'
161800040119     C                   eval      cmzmbd = vlrhdl
161900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162000040119     C***                if        prmfir = *blanks
162100040119     C                   eval      cmzfla = 'FIVAT00F'
162200040119     C                   eval      cmzmba = 'FIVAT00F'
162300040119     C***                else
162400040119     C***                eval      cmzfla = prmfir
162500040119     C***                eval      cmzmba = prmfir
162600040119     C***                endif
162700040119     C                   eval      cmznrr = *zeros
162800040119     C                   move      §ctrokvt      cmznrr
162900040119     C                   eval      cmzlba = vlrfl1
163000040119     C                   call(e)   'TIS711C'
163100040119     C                   parm                    dscmz
163200040119     C                   parm      *blanks       esito
163300040119     C                   if        %error
163400040119     C                             or cmzerr = '1'
163500040119     C                             or esito  = '1'
163600040119     C                   eval      wrkesito = '3'
163700040119     C                   else
163800040119     C*
163900040119     C* 2° invio FIVAB
164000040119     C                   reset                   dscmz
164100040119     C                   move      vlrpoi        cmzdst
164200040119     C                   eval      cmzfld = vlrfou
164300040119     C                   eval      cmzmbd = vlrhdl
164400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164500040119     C***                if        prmfir = *blanks
164600040119     C                   eval      cmzfla = 'FIVAB00F'
164700040119     C                   eval      cmzmba = 'FIVAB00F'
164800040119     C***                else
164900040119     C***                eval      cmzfla = prmfir
165000040119     C***                eval      cmzmba = prmfir
165100040119     C***                endif
165200040119     C                   eval      cmznrr = *zeros
165300040119     C                   move      §ctrokvb      cmznrr
165400040119     C                   eval      cmzlba = vlrfl1
165500040119     C                   call(e)   'TIS711C'
165600040119     C                   parm                    dscmz
165700040119     C                   parm      *blanks       esito
165800040119     C                   if        %error
165900040119     C                             or cmzerr = '1'
166000040119     C                             or esito  = '1'
166100040119     C                   eval      wrkesito = '3'
166200040119     C                   endif
166300040119     C                   endif
166400040119     C*
166500040119     C                   ENDSR
166600040119     C***
166700010601
166800010601
166900010601
167000010601
167100010601      /TITLE Invio dei dati al punto operativo.
167200010601     C     opeini        BEGSR
167300010601     C*
167400010601     C* Inizializzo flag e contatori operativi
167500010601     C                   movel     '0'           flgGiro           1
167600010601     C                   movel     '0'           flgMulti          1
167700010601     C                   movel     '1'           flgStato          1
167800010615     C                   movel     '0'           flgOk             1
167900010601     C                   z-add     *zeros        cntNonEl         10 0
168000010601     C                   z-add     *zeros        depfil            3 0
168100010601     C                   z-add     *zeros        invfil            3 0
168200010601     C*
168300010601     C                   ENDSR
168400010601     C***
168500070326
168600070326
168700070326
168800070326
168900070326     C     *pssr         BEGSR
169000070329     C*
169100070329     C                   if        %open(tivin00r)
169200070329     C                   close     tivin00r
169300070329     C                   endif
169400070329     C                   if        %open(fivabwwr)
169500070329     C                   close     fivabwwr
169600070329     C                   endif
169700070329     C                   if        %open(fivatwwr)
169800070329     C                   close     fivatwwr
169900070329     C                   endif
170000070326     C*
170100070326     C* Effettuo la chiamata al CLLE preposto
170200070326     C                   call(e)   'TITVVTC'
170300070326     C                   parm                    parccm
170400070326     C                   parm                    parmbr
170500070326     C                   parm      '2'           paropz
170600070326     C*
170700070326     C                   eval      wrkesito = '2'
170800070404     C*
170900070404     C                   seton                                        LR
171000070326     C*
171100070326     C                   ENDSR     '*CANCL'
171200070326     C***
171300100318
171400100318
171500100318
171600100318     C     normCSV       BEGSR
171700100318     C*
171800100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
171900100318     C* separatori campi
172000100318     C                   if        CharTXT <> *blanks
172100100318     C                   z-add     *zeros        wLenVINDTA        4 0
172200100318     C                   z-add     *zeros        wFlgVINDTA        1 0
172300100318     C                   setoff                                       80
172400100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
172500100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
172600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
172700100318     C                   if        not *in80
172800100318     C                   seton                                        80
172900100318     C                   else
173000100318     C                   setoff                                       80
173100100318     C                   endif
173200100318     C                   endif
173300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173400100318     C                             and *in80 = *on
173500100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
173600100318     C                   endif
173700100318     C                   enddo
173800100318     C                   endif
173900100318     C*
174000100318     C                   ENDSR
174100100318     C***
174200070326
174300100326
174400100326
174500100326     C*----------------------------------------------------*
174600100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
174700100326     C*----------------------------------------------------*
174800100326     C     REPNSP        BEGSR
174900100326     C*
175000100326     C* NSP => Stacco un numeratore da AZNUM
175100100326     C                   clear                   TRUL33DS
175200100326     C                   eval      I33OPE = *zeros
175300100326     C                   eval      I33CNU = 302
175400100326     C                   eval      I33NUM = 1
175500100326     C                   movel     TRUL33DS      KPJBU
175600100326     C                   call      'TRUL33R'
175700100326     C                   parm                    KPJBA
175800100326     C                   movel     KPJBU         TRUL33DS
175900100326     C                   if        O33ERR = *zeros
176000100326     C                   z-add     O33NRF        VABNSP
176100100326     C                   z-add     O33NRF        VATNSP
176200100326     C                   else
176300100326     C                   SETON                                        31
176400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
176500100326     C                             + ' ' + 'VABNSP VATNSP'
176600100326     C                   endif
176700100326     C*
176800100326     C                   ENDSR
176900010330
177000100326
177100010330
177200000613     C     *inzsr        BEGSR
177300990910     C*
177400990910     C     *entry        plist
177500990920     C                   parm                    tivlrds
177600990921     C                   parm      wrkesito      esito
177700000724     C                   parm                    prmlit
177800000710     C                   parm                    prmfir
177900010330     C*
178000010330     C* CALCOLA LA DATA CORRENTE
178100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
178200100318     C                   eval      datcor = %dec(%date() : *ISO)
178300000613     C*
178400000613     C                   ENDSR
178500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178600120713VATNOT_E;W_VABNSP;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_B;VABCAS;VABPKB;
178700120713W_VABNOT;VATNOT_I;X
178800120604
178900120604
179000120604
