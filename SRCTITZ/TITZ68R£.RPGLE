000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120604      *
000300120604      * PARTICOLARITA':
000400130222      * Concatenare VABRSD + VABRD2 e dividere nei due campi
000500130222      * Occorre attribuire ad ogni spedizione un VABNSP e VABRMN univoco nell?anno.
000600120604
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller) BNDDIR('UBBNDDIR')
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
004100130122     D Intestazioni    s               n   inz(*on)
004200100326     D*------------------
004300100326     D* DS REPERIMENTO NUMERATORE
004400100326     D*------------------
004500100326     D trul33ds      e ds                  inz
004600100326     D*------------------
004700100326     D* DS ARCHITETTURA
004800100326     D*------------------
004900100326     D kpjba         e ds                  inz
005000100326
005100030715     D*------------------
005200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005300030715     D*------------------
005400030715     D SkSplitFLD      S             10    DIM(100)
005500030715     D SkSplitCSV      S            256    DIM(100)
005600030715     D CharCSV         S              1
005700030715     D CharTXT         S              1
005800030715     D CharNUM         S              1
005900100318     D CharSOS         S              1
006000030715     D posDa           S              3  0 INZ(*zeros)
006100030715     D posA            S              3  0 INZ(*zeros)
006200030715     D i               s              3  0 INZ(1)
006300030715     D wGiro           s              1  0 INZ(*zeros)
006400070823     D wVATNOT_A       s                   LIKE(VATNOT)
006500060331     D wVATNOT_B       s                   LIKE(VATNOT)
006600060331     D wVATNOT_E       s                   LIKE(VATNOT)
006700110927     D wVATNOT_I       s                   LIKE(VATNOT)
006800120411     D wNOT            s             70
006900130222     D w70             s             70
007000130222     D wRSD            s             35
007100130222     D wRD2            s             35
007200120411     D Data8A          s              8
007300120709     D wBarcode        s             11
007400120709     D wBarcodeN       s             11s 0
007500120629     D wX              s              3s 0
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
009400010330
009500000913     C                   reset                   rrnum
009600990921     C                   reset                   esito
009700990921     C                   reset                   wrkesito
009800010601     C*
009900010601     C                   exsr      opeini
010000010605     C                   exsr      rwvab
010100010601     C*
010200010601     C                   seton                                        lr
010300010601
010400010601
010500010601
010600010601     C*--------------------------------------------------------
010700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010800010601     C*--------------------------------------------------------
010900010601     C     PREELA        BEGSR
011000010601     C*
011100010601     C* SE OCCORRE SPEDIRE IN FILIALE
011200010601     C                   if        invfil <> *zeros and
011300010601     C                             flgGiro = '0'
011400010601     C*
011500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011600010601     C                   eval      flgGiro = '1'
011700010601     C*
011800010601     C                   endif
011900010601     C*
012000010601     C                   ENDSR
012100010601     C***
012200010601
012300010601
012400010601
012500010601
012600010601     C*--------------------------------------------------------
012700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012800010601     C*--------------------------------------------------------
012900010601     C     ENDELA        BEGSR
013000040119     C*
013100040119     C* Effettuo la chiamata al CLLE preposto
013200040119     C                   call(e)   'TITVVTC'
013300040510     C                   parm                    parccm
013400040119     C                   parm                    parmbr
013500040119     C                   parm      '2'           paropz
013600050201     C*
013700050201     C* Effettuo lancio TISI95 solo x chiusura
013800050201     C                   CLEAR                   TISI95DS
013900050201     C                   EVAL      I95TLA = 'C'
014000050201     C                   CALL      'TISI95R'
014100050201     C                   PARM                    TISI95DS
014200000616     C*
014300010601     C                   ENDSR
014400010601
014500010601
014600010330     C*--------------------------------------------------------
014700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014800010330     C*--------------------------------------------------------
014900010605     C     RWVAB         BEGSR
015000010330     C*
015100010330     C                   if        not %open(tivin00r)
015200010330     C                   open      tivin00r
015300010330     C                   endif
015400021025     C                   if        not %open(fivabwwr)
015500021025     C                   open      fivabwwr
015600010330     C                   endif
015700040119     C*
015800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015900040119     C                   exsr      prevat
016000040119     C*
016100040119     C                   if        chkcall = '0'
016200040119     C*
016300040119     C                   if        not %open(fivatwwr)
016400040119     C                   open      fivatwwr
016500040119     C                   endif
016600010330     C*
016700040119     C                   clear                   §CTROKVB          7 0
016800040119     C                   clear                   §CTROKVT          7 0
016900040119     C                   clear                   §CTRMO            7 0
017000040119     C                   clear                   §CTRNO            7 0
017100120410     C*
017200120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017300120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017400120410     C                   IF        Intestazioni = *off
017500120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017600120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017700120410     C                   exsr      inzvar
017800120410     C                   exsr      impvab
017900120410     C                   ENDIF
018000010330     C*
018100030822     C                   do        *HIVAL
018200010330     C*
018300030822     C                   read      tivin00r                               70
018400010618     C*
018500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018600010618     C                   if        vinflg = '1'
018700010618     C                   eval      flgOk = '1'
018800010618     C                   endif
018900040510     C*
019000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019100040510     C                   z-add     *zeros        wLenVINDTA        4 0
019200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019600040510     C                   z-add     1             wFlgVINDTA
019700040510     C                   leave
019800040510     C                   endif
019900040510     C                   enddo
020000010618     C*
020100040510     C                   if        vindta > *blanks AND
020200040510     C                             wFlgVINDTA = 1
020300010330     C                   add       1             rrnum
020400010330     C*
020500010601     C                   if        *in70 = *off and
020600010330     C                             (vinflg = *blanks
020700010330     C                              or vinflg = '0'
020800010330     C                              or vinflg = '2')
020900010330     C*
021000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021100010711     C                   if        vinflg = *blanks or vinflg = '0'
021200010711     C                   clear                   vinmsg
021300010711     C                   endif
021400010601     C*
021500070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021600070925     C                   eval      vindta  = ' ' + %trim(vindta)
021700070925     C*
021800080527     C                   exsr      inzvar
021900100318     C                   exsr      normCSV
022000010605     C                   exsr      impvab
022100010601     C*
022200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022300010605     C                   eval      depfil = VABLNP
022400010601     C                   exsr      repfil
022500010601     C                   if        depfil = invfil
022600021025     C                   if        vlrpoi = 999
022700030822     C                   move(P)   invfil        VABFGS
022800021025     C                   else
022900030822     C                   move(P)   vlrpoi        VABFGS
023000021025     C                   endif
023100010601     C*
023200010601     C                   exsr      PREELA
023300010601     C*
023400010604     C* Ebbene...
023500010604     C*
023600040119     C  N31              add       1             §CTROKVB
023700040119     C   32              add       1             §CTRMO
023800040119     C   31              add       1             §CTRNO
023900080418     C*
024000030822     C                   if        wGiro = 2
024100080513     C* Scrivo sempre il VAT
024200080513     C  N31              exsr      wrivat
024300110201     C                   exsr      chkvab
024400080513     C                   endif
024500010604     C*
024600010604     C                   if        *in31 = *off and
024700010604     C                             *in32 = *off
024800010604     C                   eval      vinflg = '1'
024900010604     C                   else
025000010604     C                   eval      vinflg = '2'
025100010604     C                   endif
025200010604     C                   endif
025300010601     C*
025400010604     C                   endif
025500010604     C*
025600010330     C                   else
025700010330     C                   eval      vinflg = '1'
025800010330     C                   endif
025900010601     C*
026000010601     C  N70              update    tivin000
026100010330     C*
026200030822     C  N70              enddo
026300040119     C*
026400080527     C* Verifico dati rimasti "in cannna"...
026500080527     C                   seton                                        65
026600110201     C                   exsr      chkvab
026700080527     C*
026800040119     C                   endif                                                  (endif - chkcall)
026900010601     C*
027000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027100010601     C                   if        cntNonEl = *zeros or
027200010601     C                             flgMulti = '0'
027300010330     C* Se non ci sono record con errori ...
027400010601     C                   if        §ctrno = 0 and
027500010604     C                             §ctrmo = 0 and
027600010601     C                             flgStato <> '2'
027700010330     C* ... restituisco esito OK.
027800010330     C                   eval      wrkesito = '0'
027900010330     C                   else
028000040119     C                   if        §ctrokvb > 0
028100010330     C                   eval      wrkesito = '1'
028200010330     C                   else
028300010615     C                   if        flgOk = '0'
028400010615     C                   eval      wrkesito = '2'
028500010615     C                   else
028600010615     C                   eval      wrkesito = '6'
028700010615     C                   endif
028800010330     C                   endif
028900010330     C                   endif
029000010601     C                   else
029100010601     C                   eval      wrkesito = '9'
029200010601     C                   endif
029300010330     C*
029400010330     C                   if        %open(tivin00r)
029500010330     C                   close     tivin00r
029600010330     C                   endif
029700021025     C                   if        %open(fivabwwr)
029800021025     C                   close     fivabwwr
029900010330     C                   endif
030000040512     C                   if        %open(fivatwwr)
030100040512     C                   close     fivatwwr
030200040512     C                   endif
030300010601     C*
030400010601     C                   if        vlrpoi <> 999
030500010601     C                   eval      invfil = vlrpoi
030600010601     C                   endif
030700010330     C*
030800040119     C                   if        §ctrokvb > 0
030900010601     C                             and invfil > *zeros
031000010330     C                   exsr      invio
031100010330     C                   endif
031200010601     C*
031300010618     C                   if        flgGiro = '1'
031400010601     C                   exsr      endela
031500010618     C                   endif
031600010330     C*
031700010330     C                   ENDSR
031800010330     C***
031900080527
032000080527
032100080527
032200080527     C*----------------------------------------------------*
032300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032400080527     C*----------------------------------------------------*
032500100413     C     CHKVAB        BEGSR
032600110201     C*
032700110201     C* Se richiesti ignorare colli / peso / volume
032800110315 xxx C***                if        *in35
032900110315 xxx C***                z-add     *zeros        VABNCL
033000110315 xxx C***                z-add     *zeros        VABPKB
033100110315 xxx C***                z-add     *zeros        VABVLB
033200110315 xxx C***                endif
033300080527     C*
033400080527     C* Scrivo il VAB solo a rottura d spedizione
033500080527     C                   if        depspe = *zeros
033600080527     C  N31              eval      fivabds_sav = fivabds
033700080527     C                   eval      depspe = vabNSP
033800080527     C                   else
033900080527     C                   if        depspe = vabNSP AND *in65 = *off
034000120703     C   51              add       VABNCL        sav_VABNCL
034100100413     C   51              add       VABPKB        sav_VABPKB
034200100413     C   51              add       VABVLB        sav_VABVLB
034300100720     C   52              z-add     VABNCL        sav_VABNCL
034400100720     C   52              z-add     VABPKB        sav_VABPKB
034500100720     C   52              z-add     VABVLB        sav_VABVLB
034600100720     C   53              add       VABNCL        sav_VABNCL
034700100720     C   53              z-add     VABPKB        sav_VABPKB
034800100720     C   53              z-add     VABVLB        sav_VABVLB
034900080527     C                   else
035000080527     C  N31              eval      fivabds_ok = fivabds_sav
035100080527     C  N31              exsr      wrivab
035200080527     C  N31              eval      fivabds_sav = fivabds
035300080527     C                   eval      depspe = vabNSP
035400080527     C                   endif
035500080527     C                   endif
035600080527     C*
035700080527     C                   ENDSR
035800040119
035900040119
036000040119
036100040119     C*----------------------------------------------------*
036200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036300040119     C*----------------------------------------------------*
036400040510     C     WRIVAB        BEGSR
036500080513     C*
036600080513     C* Gestisco l'eventuale rottura x numero spedizione
036700080527     C                   if        VABNSP <> depspe or *in65 = *on
036800070301     C                   if        wVATNOT_E <> *blanks
036900070301     C                   eval      VABCTM = '7Q'
037000070301     C                   endif
037100100413     C                   write     fivab000
037200080513     C                   endif
037300040119     C*
037400040119     C                   ENDSR
037500040119
037600040119
037700040119
037800040119     C*----------------------------------------------------*
037900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038000040119     C*----------------------------------------------------*
038100040510     C     WRIVAT        BEGSR
038200060512     C*
038300060512     C* Inizializzo i campi chiave
038400060512     C                   eval      VATFGS = VABFGS
038500060512     C                   eval      VATAAS = VABAAS
038600060512     C                   eval      VATCCM = VABCCM
038700070823     C                   eval      VATNRS = VABNRS
038800060512     C                   eval      VATNSP = VABNSP
038900060512     C                   eval      VATLNP = VABLNP
039000040119     C*
039100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039200040510     C                   if        VATTRC = *blanks
039300040510     C                   eval      VATTRC = 'E'
039400040510     C                   endif
039500110318     C                   if        VATNOT <> *blanks
039600040510     C                   write     FIVAT000
039700040119     C                   add       1             §CTROKVT
039800110318     C                   endif
039900060331     C*
040000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040100060331     C                   if        wVATNOT_A <> *blanks
040200060331     C                   eval      VATNOT = wVATNOT_A
040300060331     C                   eval      VATTRC = 'A'
040400060331     C                   write     FIVAT000
040500060331     C                   add       1             §CTROKVT
040600060331     C                   endif
040700060331     C*
040800060331     C                   if        wVATNOT_B <> *blanks
040900060331     C                   eval      VATNOT = wVATNOT_B
041000060331     C                   eval      VATTRC = 'B'
041100060331     C                   write     FIVAT000
041200060331     C                   add       1             §CTROKVT
041300060331     C                   endif
041400060331     C*
041500060331     C                   if        wVATNOT_E <> *blanks
041600060331     C                   eval      VATTRC = 'E'
041700120629     C*
041800130222     C* NON sviluppo i "CHI SONO" perché non gestito
041900120703     C*
042000120711     C                   EVAL      VATNOT = wVATNOT_E
042100120629     C                   write     FIVAT000
042200120629     C                   add       1             §CTROKVT
042300120604     C*
042400120604     C                   endif
042500110927     C*
042600110927     C                   if        wVATNOT_I <> *blanks
042700110927     C                   eval      VATNOT = wVATNOT_I
042800110927     C                   eval      VATTRC = 'I'
042900110927     C                   write     FIVAT000
043000110927     C                   add       1             §CTROKVT
043100110927     C                   endif
043200040119     C*
043300040119     C                   ENDSR
043400040119
043500040119
043600040119
043700040119     C*----------------------------------------------------*
043800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043900040119     C*----------------------------------------------------*
044000040119     C     PREVAT        BEGSR
044100040119     C*
044200040119     C* Compongo il nome del membro da dare al FIVATWWR
044300040119     C                   eval      parmbr = vlrhdl
044400040119     C                   movel     'M'           parmbr
044500040510     C                   eval      parccm = vlrksc
044600040119     C                   eval      paropz = '1'
044700040119     C* Effettuo la chiamata al CLLE preposto
044800040119     C                   call(e)   'TITVVTC'
044900040119     C                   parm                    parccm
045000040119     C                   parm                    parmbr
045100040119     C                   parm                    paropz
045200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045300040119     C                   if        %error
045400040119     C                   movel     '1'           chkcall
045500040119     C                   else
045600040119     C                   movel     '0'           chkcall
045700040119     C                   endif
045800040119     C*
045900040119     C                   ENDSR
046000010601
046100010601
046200010601
046300010330     C*----------------------------------------------------*
046400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046500010330     C*----------------------------------------------------*
046600040119     C     INZVAR        BEGSR
046700080527     C*
046800080527     C                   setoff                                       65
046900010330     C*
047000030822     C* Inizializzo variabili di wrk
047100020204     C                   Z-ADD     *zeros        Num5_0            5 0
047200101018     C                   MOVEL     *blanks       wFlgCAS
047300060331     C                   MOVEL     *blanks       wVATNOT_A
047400060331     C                   MOVEL     *blanks       wVATNOT_B
047500060331     C                   MOVEL     *blanks       wVATNOT_E
047600110927     C                   MOVEL     *blanks       wVATNOT_I
047700120411     C                   MOVEL     *blanks       wNOT
047800120411     C                   MOVEL     *blanks       Data8A
047900030822     C*
048000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048100030822     C                   CLEAR                   SkSplitCSV
048200030822     C                   EVAL      i = 1
048300030822     C                   EVAL      posDa = *zeros
048400030822     C                   EVAL      posA  = *zeros
048500080527     C*
048600080527     C                   CLEAR                   FIVABDS
048700080527     C                   CLEAR                   FIVAT000
048800030822     C*
048900030822     C* Reimposto i valori di default
049000030822     C                   EXSR      DEFCAM
049100030822     C*
049200010330     C                   ENDSR
049300010330     C*----------------------------------------------------*
049400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049500010330     C*----------------------------------------------------*
049600010330     C     DEFCAM        BEGSR
049700030822     C*
049800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049900030715     C* e delimitatore testo.
050000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050300100318     C*
050400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050500100318     C                   EVAL      CharSOS = CharNUM
050600030822     C*
050700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050800100720     C                   SETOFF                                       505152
050900100720     C                   SETOFF                                       53
051000100413     C                   SELECT
051100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051200100720     C                   SETON                                        50
051300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051400100413     C                   SETON                                        51
051500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051600100720     C                   SETON                                        52
051700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051800100720     C                   SETON                                        53
051900100413     C                   ENDSL
052000030822     C*
052100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052200030822     C                   EVAL      posDaDft = 1
052300030822     C                   EVAL      posADft  = 0
052400030822     C                   EVAL      wGiroDft = 0
052500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052600030822     C                             posDaDft > 0
052700030822     C*
052800030822     C* Gestisco il 1° giro
052900030822     C                   IF        wGiroDft = 0
053000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053200030822     C* Incremento il contatore dei "giri"
053300030822     C                   EVAL      wGiroDft = 1
053400030822     C                   ELSE
053500030822     C                   EVAL      posDaDft = posADft
053600030822     C                   ENDIF
053700030822     C* Eseguo lo scan x trovare la fine del campo corrente
053800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053900030822     C*
054000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054200030822     C                   IF        posDaDft > 0 AND
054300030822     C                             posADft  > 0
054400030822     C* NCL
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'NCL'
054900030822     C                   EVAL      PiStr=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   EXSR      CHKNUM
055300030822     C                   IF        PiInt=*on
055400030822     C                   Z-ADD     PiVal         VABNCL
055500030822     C                   ENDIF
055600030822     C                   ENDIF
055700030822     C* CCM
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'CCM'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiInt=*on
056700030822     C                   Z-ADD     PiVal         VABCCM
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* LNP
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'LNP'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiInt=*on
058000030822     C                   Z-ADD     PiVal         VABLNP
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* NRS
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'NRS'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiInt=*on
059300030822     C                   Z-ADD     PiVal         VABNRS
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* CTR
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'CTR'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600030822     C                   Z-ADD     PiVal         VABCTR
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* PKB
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'PKB'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiNum=*on
061900120418     C                   Z-ADD     PiVal         VABPKB
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* VLB
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'VLB'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiNum=*on
063200030822     C                   Z-ADD     PiVal         VABVLB
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* QFT
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'QFT'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiNum=*on
064500030822     C                   Z-ADD     PiVal         VABQFT
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* CBO
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'CBO'
065300030822     C                   EVAL      VABCBO=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* TSP
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'TSP'
066200030822     C                   EVAL      VABTSP=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* VAS
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'VAS'
067100030822     C                   EVAL      VABVAS=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* VCA
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'VCA'
068000030822     C                   EVAL      VABVCA=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400070925     C* TIC
068500070925     C                   IF        %subst(
068600070925     C                             %subst(vlrppt:posDaDft+1:
068700070925     C                             posADft-posDaDft-1):1:3)
068800070925     C                             = 'TIC'
068900070925     C                   EVAL      VABTIC=%trim(%subst(
069000070925     C                             %subst(vlrppt:posDaDft+1:
069100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200070925     C                   ENDIF
069300030822     C* GCA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'GCA'
069800030822     C                   EVAL      VABGCA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* CTM
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'CTM'
070700030822     C                   EVAL      VABCTM=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* FFD
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'FFD'
071600030822     C                   EVAL      VABFFD=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* VAD
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'VAD'
072500030822     C                   EVAL      VABVAD=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* GMA
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'GMA'
073400030822     C                   EVAL      VABGMA=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* GGA
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'GGA'
074300030822     C                   EVAL      VABGGA=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* GVA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'GVA'
075200030822     C                   EVAL      VABGVA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* TC1
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'TC1'
076100030822     C                   EVAL      VABTC1=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* TC2
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'TC2'
077000030822     C                   EVAL      VABTC2=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400040714     C* VATTRC
077500040714     C                   IF        %subst(
077600040714     C                             %subst(vlrppt:posDaDft+1:
077700040714     C                             posADft-posDaDft-1):1:3)
077800040714     C                             = 'TRC'
077900040714     C                   EVAL      VATTRC=%trim(%subst(
078000040714     C                             %subst(vlrppt:posDaDft+1:
078100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200040714     C                   ENDIF
078300030822     C* ...
078400030822     C                   ENDIF
078500030822     C                   ENDDO
078600020204     C*
078700010330     C                   ENDSR
078800010607     C*----------------------------------------------------*
078900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079000010607     C*----------------------------------------------------*
079100010607     C     IMPVAB        BEGSR
079200010607     C*
079300010607     C                   SETOFF                                       3132
079400110315 xxx C***                SETON                                        35
079500030515     C*
079600010607     C                   MOVEL     datcor        VABAAS
079700010607     C                   MOVE      datcor        VABMGS
079800010607     C*
079900030715     C**********
080000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080100030715     C**********
080200030715     C                   IF        wGiro = *zeros
080300040113     C*
080400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080700080429     C                             %trim(intcol(5))<>*blanks
080800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081000080429     C                                      %trim(intcol(5))
081100070823     C     ';':CharCSV   XLATE     vindta        vindta
081200040510     C                   ENDIF
081300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081400031201     C     minu:maiu     XLATE     vindta        vindta
081500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081600030715     C                   DOW       posDa <= %len(%trim(vindta))
081700030715     C*
081800030715     C* Gestisco il 1° campo
081900030715     C                   IF        i = 1
082000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082100030715     C                   EVAL      posDa = 1
082200030715     C* Eseguo lo scan x trovare la fine del primo campo
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* A questo punto "estraggo" il campo corrente
082500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082600030715     C                                                    (posA-posDa))
082700030715     C* X i campi successivi al 1°
082800030715     C                   ELSE
082900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083000030715     C                   EVAL      posDa = posA + 1
083100030715     C* Eseguo lo scan x trovare la fine del campo corrente
083200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083300030715     C* Gestisco l'ultimo campo
083400030715     C                   IF        posA = *zeros
083500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C                   LEAVE
084000030715     C                   ELSE
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   ENDIF
084500030715     C                   ENDIF
084600030715     C* Incremento il contatore d campo
084700030715     C                   EVAL      i = i +1
084800030715     C                   ENDDO
084900030715     C*
085000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085100030715     C* delimitazione testo
085200030715     C                   EVAL      i = 1
085300030715     C                   DOW       i < %elem(SkSplitFLD)
085400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085600030715     C                   EVAL      i = i + 1
085700030715     C                   ENDDO
085800030715     C                   EVAL      wGiro = 1
085900030715     C                   ELSE
086000030715     C                   EVAL      wGiro = 2
086100031204     C**********
086200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086300031204     C* d separatore campo
086400031204     C**********
086500031204     C                   EVAL      vindta = %trim(vindta)
086600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086700031204     C                   EVAL      vindta = %subst(vindta:2)
086800031204     C                   ENDDO
086900031204     C*
087000031204     C                   Z-ADD     *zeros        lunghInput        4 0
087100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500110912     C                   ENDDO
087600030715     C**********
087700030715     C* Effettuo lo split del campo dati d input
087800030715     C**********
087900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088000030715     C                   DOW       posDa <= %len(%trim(vindta))
088100030715     C*
088200030715     C* Gestisco il 1° campo
088300030715     C                   IF        i = 1
088400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088500030715     C                   EVAL      posDa = 1
088600030715     C* Eseguo lo scan x trovare la fine del primo campo
088700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C* X i campi successivi al 1°
089200030715     C                   ELSE
089300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089400030715     C                   EVAL      posDa = posA + 1
089500031204     C* Verifico che nn vi sia il campo nn valorizzato
089600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089700031204     C* Se campo nn valorizzato skippo al prossimo
089800031204     C                   EVAL      SkSplitCSV(i) = *blanks
089900031204     C                   EVAL      posA  = posA + 1
090000031204     C                   ELSE
090100030715     C* Eseguo lo scan x trovare la fine del campo corrente
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* Gestisco l'ultimo campo
090400030715     C                   IF        posA = *zeros
090500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   LEAVE
091000030715     C                   ELSE
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   ENDIF
091500031204     C                   ENDIF
091600030715     C                   ENDIF
091700030715     C* Incremento il contatore d campo
091800030715     C                   EVAL      i = i +1
091900030715     C                   ENDDO
092000030715     C*
092100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092200030715     C* delimitazione testo
092300030715     C                   EVAL      i = 1
092400030715     C                   DOW       i < %elem(SkSplitCSV)
092500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092700030715     C                   EVAL      i = i + 1
092800030715     C                   ENDDO
092900100720     C*
093000130222     C* Stacco un progressivo x numero spedizione
093100130222     C                   EXSR      repNSP
093200100326     C*
093300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093400030715     C                   EVAL      i = 1
093500030715     C                   DOW       i < %elem(SkSplitFLD)
093600030715     C*
093700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093900030715     C                             SkSplitCSV(i) <> *blanks
094000030721     C*** ATB
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094200030721     C                   EVAL      VABATB = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030715     C*** RSD
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094900030715     C* ==
095000030715     C                   ENDIF
095100030721     C*** RD2
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030715     C*** IND
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095700030715     C                   EVAL      VABIND = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** LOD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** PRD
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030721     C*** NZD
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030715     C*** RMA
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** NOT
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** NT2
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** VCA
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** VAS
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** FFD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030721     C*** GC1
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** GC2
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** TSP
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100600030721     C                   ENDIF
100700040510     C*** CBO
100800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101000040510     C                   ENDIF
101100030721     C*** NAS
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** TIC
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** GCA
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** XCO
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** CTM
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** TCR
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** CTS
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** FTM
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** VAD
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** GMA
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GGA
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GVA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TC1
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** TC2
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** SCL
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** RMO
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** NMO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*
108000030721     C* Reperisco quindi i campi numerici...
108100040510     C*** NSP
108200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108300040510     C                   EVAL      PiStr=SkSplitCSV(i)
108400040510     C                   EXSR      CHKNUM
108500040510     C                   IF        PiInt=*on
108600040510     C                   Z-ADD     PiVal         VABNSP
108700040510     C                   ELSE
108800040510     C                   SETON                                        31
108900040510     C                   EVAL      VABNSP = *zeros
109000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109100040510     C                             + ' ' + 'VABNSP'
109200040510     C                   ENDIF
109300040510     C                   ENDIF
109400071106     C*** RMN
109500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109600071106     C                   EVAL      PiStr=SkSplitCSV(i)
109700071106     C                   EXSR      CHKNUM
109800071106     C                   IF        PiInt=*on
109900071106     C                   Z-ADD     PiVal         VABRMN
110000071106     C                   ELSE
110100071106     C                   SETON                                        32
110200071106     C                   EVAL      VABRMN = *zeros
110300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110400071106     C                             + ' ' + 'VABRMN'
110500071106     C                   ENDIF
110600071106     C                   ENDIF
110700030721     C*** CAD
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on
111200030721     C                   Z-ADD     PiVal         Num5_0
111300030721     C                   MOVEL(p)  Num5_0        VABCAD
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABCAD = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABCAD'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** NCL
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on
112600030721     C                   Z-ADD     PiVal         VABNCL
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABNCL = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040122     C                             + ' ' + 'VABNCL'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** PKB
113500120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
113600120418     C* per cui si arrotonda il valore passato agli etti
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiNum=*on
114100120418     C                   Z-ADD(H)  PiVal         VABPKB
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      VABPKB = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABPKB'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** CAS
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115100110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115200111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115300120713     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
115400120713     C                             %trim(SkSplitCSV(i)) <> '0,00'
115500120605     C                   EVAL      VABVCA = 'EUR'
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
124600130222     C                   EVAL      vabVAS = 'EUR'
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABIAS'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** VLB
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiNum=*on
125800030721     C                   Z-ADD     PiVal         VABVLB
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABVLB'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** QFT
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiNum=*on
127000030721     C                   Z-ADD     PiVal         VABQFT
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABQFT'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** NCD
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on      AND
128200030721     C                             PiVal<=9999999
128300030721     C                   Z-ADD     PiVal         VABNCD
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABNCD'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** NCA
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiInt=*on      AND
129500030721     C                             PiVal<=9999999
129600030721     C                   Z-ADD     PiVal         VABNCA
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABNCA'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** ZNC
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiInt=*on AND
130800030721     C                             PiVal<=99
130900030721     C                   Z-ADD     PiVal         VABZNC
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABZNC'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** DCR
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on       AND
132100030721     C                             PiVal<=99999999
132200030721     C                   Z-ADD     PiVal         VABDCR
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABDCR'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** HCR
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiInt=*on   AND
133400030721     C                             PiVal<=9999
133500030721     C                   Z-ADD     PiVal         VABHCR
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABHCR'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** VMD
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiNum=*on
134700030721     C                   Z-ADD     PiVal         VABVMD
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABVMD'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** ANT
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiInt=*on        AND
135900030721     C                             PiVal<=999999999
136000030721     C                   Z-ADD     PiVal         VABANT
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABANT'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** CMO
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiInt=*on
137200030721     C                   Z-ADD     PiVal         Num5_0
137300030721     C                   MOVEL(p)  Num5_0        VABCMO
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      VABCMO = *zeros
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABCMO'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100040510     C*
138200040510     C* Reperisco i campi del FIVAT
138300040510     C*** VATNOT
138400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138600040510     C                   ENDIF
138700040510     C*** VATTRC
138800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139000040510     C                   ENDIF
139100060331     C*** VATNOT_A
139200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139400060331     C                   ENDIF
139500060331     C*** VATNOT_B
139600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139800060331     C                   ENDIF
139900060331     C*** VATNOT_E
140000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140200060331     C                   ENDIF
140300110927     C*** VATNOT_I
140400110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140500110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140600110927     C                   ENDIF
140700070823     C*
140800120713 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
140900130122     C*
141000130222     C*** W_VABRSD (devo concatenarlo con W_VABRD2)
141100130222     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141200130222     C                   EVAL      wRSD = %trim(SkSplitCSV(i))
141300130222     C                   ENDIF
141400130222     C*
141500130222     C*** W_VABRD2 (devo concatenarlo con W_VABRSD)
141600130222     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRD2'
141700130222     C                   EVAL      wRD2 = %trim(SkSplitCSV(i))
141800130222     C                   ENDIF
141900130222     C*
142000130222     C*** W_VABRMA è sia VABRMA che wVATNOT_A
142100130222     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
142200130222     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
142300130222     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142400130222     C                   ENDIF
142500120713
142600120301
142700070925     C*
142800071029     C***  <===  -----------------------------------------
142900040510     C*
143000030715     C                   ENDIF
143100030715     C                   EVAL      i = i + 1
143200030715     C                   ENDDO
143300070925     C*
143400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
143500120411     C*
143600130122     C*** VABRMN è = VABNSP
143700130122     C                   EVAL      VABRMN = VABNSP
143800130222     C*
143900130222     C*** concatenare VABRSD + VABRD2 e dividere nei due campi
144000130222     C                   EVAL      w70 =
144100130222     C                              %trim(wRSD) + ' ' + %trim(wRD2)
144200130222     C                   EVAL      VABRSD =
144300130222     C                              %subst(w70
144400130222     C                               : 1 : 35)
144500130222     C                   EVAL      VABRD2 =
144600130222     C                              %subst(w70
144700130222     C                               : 36 : 35)
144800070925     C*
144900071029     C***  <===  -----------------------------------------
145000070925     C*
145100070925     C* Considerazioni finali su CBO/CAS
145200100318     C                   IF        wFlgCAS = '1'    OR
145300100318     C                             VABCAS  > *zeros
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
179200130222W_VABRSD;W_VABRD2;VABLOD;VABIND;VABCAD;VABPRD;VABNCL;VABPKB;VABNOT;VABNT2;
179300130222VABIAS;W_VABRMA;VATNOT_B;VABNAS;VABRMO;
179400120604
179500120604
179600120604
