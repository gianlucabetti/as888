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
047800140326     C                   MOVEL     *blanks       w70
047900140326     C                   MOVEL     *blanks       wRSD
048000140326     C                   MOVEL     *blanks       wRD2
048100120411     C                   MOVEL     *blanks       Data8A
048200030822     C*
048300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048400030822     C                   CLEAR                   SkSplitCSV
048500030822     C                   EVAL      i = 1
048600030822     C                   EVAL      posDa = *zeros
048700030822     C                   EVAL      posA  = *zeros
048800080527     C*
048900080527     C                   CLEAR                   FIVABDS
049000080527     C                   CLEAR                   FIVAT000
049100030822     C*
049200030822     C* Reimposto i valori di default
049300030822     C                   EXSR      DEFCAM
049400030822     C*
049500010330     C                   ENDSR
049600010330     C*----------------------------------------------------*
049700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049800010330     C*----------------------------------------------------*
049900010330     C     DEFCAM        BEGSR
050000030822     C*
050100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050200030715     C* e delimitatore testo.
050300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050600100318     C*
050700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050800100318     C                   EVAL      CharSOS = CharNUM
050900030822     C*
051000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051100100720     C                   SETOFF                                       505152
051200100720     C                   SETOFF                                       53
051300100413     C                   SELECT
051400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051500100720     C                   SETON                                        50
051600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051700100413     C                   SETON                                        51
051800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051900100720     C                   SETON                                        52
052000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052100100720     C                   SETON                                        53
052200100413     C                   ENDSL
052300030822     C*
052400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052500030822     C                   EVAL      posDaDft = 1
052600030822     C                   EVAL      posADft  = 0
052700030822     C                   EVAL      wGiroDft = 0
052800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052900030822     C                             posDaDft > 0
053000030822     C*
053100030822     C* Gestisco il 1° giro
053200030822     C                   IF        wGiroDft = 0
053300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053500030822     C* Incremento il contatore dei "giri"
053600030822     C                   EVAL      wGiroDft = 1
053700030822     C                   ELSE
053800030822     C                   EVAL      posDaDft = posADft
053900030822     C                   ENDIF
054000030822     C* Eseguo lo scan x trovare la fine del campo corrente
054100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054200030822     C*
054300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054500030822     C                   IF        posDaDft > 0 AND
054600030822     C                             posADft  > 0
054700030822     C* NCL
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'NCL'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABNCL
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* CCM
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'CCM'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABCCM
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* LNP
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'LNP'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABLNP
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* NRS
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'NRS'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiInt=*on
059600030822     C                   Z-ADD     PiVal         VABNRS
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* CTR
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'CTR'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiInt=*on
060900030822     C                   Z-ADD     PiVal         VABCTR
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* PKB
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'PKB'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiNum=*on
062200120418     C                   Z-ADD     PiVal         VABPKB
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* VLB
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'VLB'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiNum=*on
063500030822     C                   Z-ADD     PiVal         VABVLB
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* QFT
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'QFT'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiNum=*on
064800030822     C                   Z-ADD     PiVal         VABQFT
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* CBO
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'CBO'
065600030822     C                   EVAL      VABCBO=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* TSP
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'TSP'
066500030822     C                   EVAL      VABTSP=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* VAS
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'VAS'
067400030822     C                   EVAL      VABVAS=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* VCA
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'VCA'
068300030822     C                   EVAL      VABVCA=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700070925     C* TIC
068800070925     C                   IF        %subst(
068900070925     C                             %subst(vlrppt:posDaDft+1:
069000070925     C                             posADft-posDaDft-1):1:3)
069100070925     C                             = 'TIC'
069200070925     C                   EVAL      VABTIC=%trim(%subst(
069300070925     C                             %subst(vlrppt:posDaDft+1:
069400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500070925     C                   ENDIF
069600030822     C* GCA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'GCA'
070100030822     C                   EVAL      VABGCA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* CTM
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'CTM'
071000030822     C                   EVAL      VABCTM=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* FFD
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'FFD'
071900030822     C                   EVAL      VABFFD=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* VAD
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'VAD'
072800030822     C                   EVAL      VABVAD=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* GMA
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'GMA'
073700030822     C                   EVAL      VABGMA=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* GGA
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'GGA'
074600030822     C                   EVAL      VABGGA=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* GVA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GVA'
075500030822     C                   EVAL      VABGVA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* TC1
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'TC1'
076400030822     C                   EVAL      VABTC1=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* TC2
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'TC2'
077300030822     C                   EVAL      VABTC2=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700040714     C* VATTRC
077800040714     C                   IF        %subst(
077900040714     C                             %subst(vlrppt:posDaDft+1:
078000040714     C                             posADft-posDaDft-1):1:3)
078100040714     C                             = 'TRC'
078200040714     C                   EVAL      VATTRC=%trim(%subst(
078300040714     C                             %subst(vlrppt:posDaDft+1:
078400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500040714     C                   ENDIF
078600030822     C* ...
078700030822     C                   ENDIF
078800030822     C                   ENDDO
078900020204     C*
079000010330     C                   ENDSR
079100010607     C*----------------------------------------------------*
079200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079300010607     C*----------------------------------------------------*
079400010607     C     IMPVAB        BEGSR
079500010607     C*
079600010607     C                   SETOFF                                       3132
079700110315 xxx C***                SETON                                        35
079800030515     C*
079900010607     C                   MOVEL     datcor        VABAAS
080000010607     C                   MOVE      datcor        VABMGS
080100010607     C*
080200030715     C**********
080300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080400030715     C**********
080500030715     C                   IF        wGiro = *zeros
080600040113     C*
080700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081000080429     C                             %trim(intcol(5))<>*blanks
081100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081300080429     C                                      %trim(intcol(5))
081400070823     C     ';':CharCSV   XLATE     vindta        vindta
081500040510     C                   ENDIF
081600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081700031201     C     minu:maiu     XLATE     vindta        vindta
081800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081900030715     C                   DOW       posDa <= %len(%trim(vindta))
082000030715     C*
082100030715     C* Gestisco il 1° campo
082200030715     C                   IF        i = 1
082300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082400030715     C                   EVAL      posDa = 1
082500030715     C* Eseguo lo scan x trovare la fine del primo campo
082600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C* X i campi successivi al 1°
083100030715     C                   ELSE
083200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083300030715     C                   EVAL      posDa = posA + 1
083400030715     C* Eseguo lo scan x trovare la fine del campo corrente
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* Gestisco l'ultimo campo
083700030715     C                   IF        posA = *zeros
083800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C                   LEAVE
084300030715     C                   ELSE
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   ENDIF
084800030715     C                   ENDIF
084900030715     C* Incremento il contatore d campo
085000030715     C                   EVAL      i = i +1
085100030715     C                   ENDDO
085200030715     C*
085300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085400030715     C* delimitazione testo
085500030715     C                   EVAL      i = 1
085600030715     C                   DOW       i < %elem(SkSplitFLD)
085700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085900030715     C                   EVAL      i = i + 1
086000030715     C                   ENDDO
086100030715     C                   EVAL      wGiro = 1
086200030715     C                   ELSE
086300030715     C                   EVAL      wGiro = 2
086400031204     C**********
086500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086600031204     C* d separatore campo
086700031204     C**********
086800031204     C                   EVAL      vindta = %trim(vindta)
086900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087000031204     C                   EVAL      vindta = %subst(vindta:2)
087100031204     C                   ENDDO
087200031204     C*
087300031204     C                   Z-ADD     *zeros        lunghInput        4 0
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087800110912     C                   ENDDO
087900030715     C**********
088000030715     C* Effettuo lo split del campo dati d input
088100030715     C**********
088200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088300030715     C                   DOW       posDa <= %len(%trim(vindta))
088400030715     C*
088500030715     C* Gestisco il 1° campo
088600030715     C                   IF        i = 1
088700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088800030715     C                   EVAL      posDa = 1
088900030715     C* Eseguo lo scan x trovare la fine del primo campo
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* A questo punto "estraggo" il campo corrente
089200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089300030715     C                                                    (posA-posDa))
089400030715     C* X i campi successivi al 1°
089500030715     C                   ELSE
089600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089700030715     C                   EVAL      posDa = posA + 1
089800031204     C* Verifico che nn vi sia il campo nn valorizzato
089900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090000031204     C* Se campo nn valorizzato skippo al prossimo
090100031204     C                   EVAL      SkSplitCSV(i) = *blanks
090200031204     C                   EVAL      posA  = posA + 1
090300031204     C                   ELSE
090400030715     C* Eseguo lo scan x trovare la fine del campo corrente
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* Gestisco l'ultimo campo
090700030715     C                   IF        posA = *zeros
090800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   LEAVE
091300030715     C                   ELSE
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   ENDIF
091800031204     C                   ENDIF
091900030715     C                   ENDIF
092000030715     C* Incremento il contatore d campo
092100030715     C                   EVAL      i = i +1
092200030715     C                   ENDDO
092300030715     C*
092400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092500030715     C* delimitazione testo
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitCSV)
092800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093000030715     C                   EVAL      i = i + 1
093100030715     C                   ENDDO
093200100720     C*
093300130222     C* Stacco un progressivo x numero spedizione
093400130222     C                   EXSR      repNSP
093500100326     C*
093600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitFLD)
093900030715     C*
094000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094200030715     C                             SkSplitCSV(i) <> *blanks
094300030721     C*** ATB
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094500030721     C                   EVAL      VABATB = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030715     C*** RSD
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095200030715     C* ==
095300030715     C                   ENDIF
095400030721     C*** RD2
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** IND
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096000030715     C                   EVAL      VABIND = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** LOD
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** PRD
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030721     C*** NZD
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030715     C*** RMA
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** NOT
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** NT2
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** VCA
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** VAS
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** FFD
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030721     C*** GC1
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** GC2
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** TSP
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100900030721     C                   ENDIF
101000040510     C*** CBO
101100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101300040510     C                   ENDIF
101400030721     C*** NAS
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TIC
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GCA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** XCO
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** CTM
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TCR
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** CTS
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** FTM
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** VAD
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GMA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** GGA
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GVA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** TC1
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** TC2
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** SCL
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** RMO
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** NMO
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*
108300030721     C* Reperisco quindi i campi numerici...
108400040510     C*** NSP
108500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108600040510     C                   EVAL      PiStr=SkSplitCSV(i)
108700040510     C                   EXSR      CHKNUM
108800040510     C                   IF        PiInt=*on
108900040510     C                   Z-ADD     PiVal         VABNSP
109000040510     C                   ELSE
109100040510     C                   SETON                                        31
109200040510     C                   EVAL      VABNSP = *zeros
109300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109400040510     C                             + ' ' + 'VABNSP'
109500040510     C                   ENDIF
109600040510     C                   ENDIF
109700071106     C*** RMN
109800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109900071106     C                   EVAL      PiStr=SkSplitCSV(i)
110000071106     C                   EXSR      CHKNUM
110100071106     C                   IF        PiInt=*on
110200071106     C                   Z-ADD     PiVal         VABRMN
110300071106     C                   ELSE
110400071106     C                   SETON                                        32
110500071106     C                   EVAL      VABRMN = *zeros
110600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110700071106     C                             + ' ' + 'VABRMN'
110800071106     C                   ENDIF
110900071106     C                   ENDIF
111000030721     C*** CAD
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiInt=*on
111500030721     C                   Z-ADD     PiVal         Num5_0
111600030721     C                   MOVEL(p)  Num5_0        VABCAD
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABCAD = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCAD'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** NCL
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on
112900030721     C                   Z-ADD     PiVal         VABNCL
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      VABNCL = *zeros
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040122     C                             + ' ' + 'VABNCL'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** PKB
113800120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
113900120418     C* per cui si arrotonda il valore passato agli etti
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiNum=*on
114400120418     C                   Z-ADD(H)  PiVal         VABPKB
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      VABPKB = *zeros
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABPKB'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** CAS
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115600120713     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
115700120713     C                             %trim(SkSplitCSV(i)) <> '0,00'
115800120605     C                   EVAL      VABVCA = 'EUR'
115900101018     C                   EVAL      wFlgCAS = '1'
116000101018     C                   ENDIF
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiNum=*on
116400030721     C                   Z-ADD     PiVal         VABCAS
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABCAS = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900030721     C                             + ' ' + 'VABCAS'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** CCM
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiInt=*on      AND
117700030721     C                             PiVal<=9999999 AND
117800030721     C                             PiVal>*zeros
117900030721     C                   Z-ADD     PiVal         VABCCM
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      VABCCM = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400040510     C                             + ' ' + 'VABCCM'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** LNP
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on    AND
119200030721     C                             PiVal<=999   AND
119300030721     C                             PiVal>*zeros
119400030721     C                   Z-ADD     PiVal         VABLNP
119500030721     C                   ELSE
119600040715     C                   SETON                                        32
119700030721     C                   EVAL      VABLNP = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040510     C                             + ' ' + 'VABLNP'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** NRS
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on AND
120700030721     C                             PiVal<=99
120800030721     C                   Z-ADD     PiVal         VABNRS
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABNRS'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** LNA
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on    AND
122000030721     C                             PiVal<=999   AND
122100030721     C                             PiVal>*zeros
122200030721     C                   Z-ADD     PiVal         VABLNA
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      VABLNA = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABLNA'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** CTR
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on  AND
123500030721     C                             PiVal<=999
123600030721     C                   Z-ADD     PiVal         VABCTR
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABCTR'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** IAS
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiNum=*on
124800030721     C                   Z-ADD     PiVal         VABIAS
124900130222     C                   EVAL      vabVAS = 'EUR'
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300030721     C                             + ' ' + 'VABIAS'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** VLB
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiNum=*on
126100030721     C                   Z-ADD     PiVal         VABVLB
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABVLB'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** QFT
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiNum=*on
127300030721     C                   Z-ADD     PiVal         VABQFT
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABQFT'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** NCD
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on      AND
128500030721     C                             PiVal<=9999999
128600030721     C                   Z-ADD     PiVal         VABNCD
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABNCD'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** NCA
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on      AND
129800030721     C                             PiVal<=9999999
129900030721     C                   Z-ADD     PiVal         VABNCA
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABNCA'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** ZNC
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on AND
131100030721     C                             PiVal<=99
131200030721     C                   Z-ADD     PiVal         VABZNC
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABZNC'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** DCR
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on       AND
132400030721     C                             PiVal<=99999999
132500030721     C                   Z-ADD     PiVal         VABDCR
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABDCR'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** HCR
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on   AND
133700030721     C                             PiVal<=9999
133800030721     C                   Z-ADD     PiVal         VABHCR
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABHCR'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** VMD
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiNum=*on
135000030721     C                   Z-ADD     PiVal         VABVMD
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABVMD'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700030721     C*** ANT
135800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on        AND
136200030721     C                             PiVal<=999999999
136300030721     C                   Z-ADD     PiVal         VABANT
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700030721     C                             + ' ' + 'VABANT'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000030721     C*** CMO
137100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137200030721     C                   EVAL      PiStr=SkSplitCSV(i)
137300030721     C                   EXSR      CHKNUM
137400030721     C                   IF        PiInt=*on
137500030721     C                   Z-ADD     PiVal         Num5_0
137600030721     C                   MOVEL(p)  Num5_0        VABCMO
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      VABCMO = *zeros
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABCMO'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400040510     C*
138500040510     C* Reperisco i campi del FIVAT
138600040510     C*** VATNOT
138700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138900040510     C                   ENDIF
139000040510     C*** VATTRC
139100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139300040510     C                   ENDIF
139400060331     C*** VATNOT_A
139500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139700060331     C                   ENDIF
139800060331     C*** VATNOT_B
139900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140100060331     C                   ENDIF
140200060331     C*** VATNOT_E
140300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140500060331     C                   ENDIF
140600110927     C*** VATNOT_I
140700110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140800110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140900110927     C                   ENDIF
141000070823     C*
141100120713 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
141200130122     C*
141300130222     C*** W_VABRSD (devo concatenarlo con W_VABRD2)
141400130222     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141500130222     C                   EVAL      wRSD = %trim(SkSplitCSV(i))
141600130222     C                   ENDIF
141700130222     C*
141800130222     C*** W_VABRD2 (devo concatenarlo con W_VABRSD)
141900130222     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRD2'
142000130222     C                   EVAL      wRD2 = %trim(SkSplitCSV(i))
142100130222     C                   ENDIF
142200130222     C*
142300130222     C*** W_VABRMA è sia VABRMA che wVATNOT_A
142400130222     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
142500130222     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
142600130222     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142700130222     C                   ENDIF
142800120713
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
143900130122     C*** VABRMN è = VABNSP
144000130122     C                   EVAL      VABRMN = VABNSP
144100130222     C*
144200130222     C*** concatenare VABRSD + VABRD2 e dividere nei due campi
144300130222     C                   EVAL      w70 =
144400130222     C                              %trim(wRSD) + ' ' + %trim(wRD2)
144500130222     C                   EVAL      VABRSD =
144600130222     C                              %subst(w70
144700130222     C                               : 1 : 35)
144800130222     C                   EVAL      VABRD2 =
144900130222     C                              %subst(w70
145000130222     C                               : 36 : 35)
145100070925     C*
145200071029     C***  <===  -----------------------------------------
145300070925     C*
145400070925     C* Considerazioni finali su CBO/CAS
145500100318     C                   IF        wFlgCAS = '1'    OR
145600100318     C                             VABCAS  > *zeros
145700070925     C                   IF        VABCBO = '1'
145800070925     C                   EVAL      VABCBO = '4'
145900070925     C                   ENDIF
146000070925     C                   IF        VABCBO = '2'
146100070925     C                   EVAL      VABCBO = '6'
146200070925     C                   ENDIF
146300070925     C                   ENDIF
146400070925     C***  <===  ----------------------------
146500040510     C*
146600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146700040510     C                   IF        VABNSP = *zeros
146800040510     C                   SETON                                        31
146900040510     C                   ENDIF
147000040715     C*
147100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147200040715     C                   IF        VABLNP = *zeros
147300040715     C                   SETON                                        31
147400040715     C                   ENDIF
147500040510     C*
147600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147700120411     C                   IF        VABRMN = *zeros
147800120411     C                   Z-ADD     VABNSP        VABRMN
147900120411     C                   ENDIF
148000120328     C* IN QUESTO CASO LO FORZO = 1
148100120411     C***                IF        VABRMN = *zeros
148200120411     C***                Z-ADD     1             VABRMN
148300120411     C***                ENDIF
148400040510     C*
148500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148600040510     C                   IF        VABRMA = *blanks
148700040714     C                   MOVEL     VABRMN        VABRMA
148800040510     C                   ENDIF
148900040128     C*
149000040510     C* Se provincia nn valorizzata la reperisco
149100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149200110912     C                   IF        VABPRD =  *blanks AND
149300110912     C                             VABNZD =  *blanks AND
149400110912     C                             VABCAD <> *blanks AND
149500110912     C                             VABLOD <> *blanks
149600040128     C                   CLEAR                   TISI95DS
149700040128     C                   EVAL      I95TCN = '3'
149800040128     C                   Z-ADD     datcor        I95DAT
149900040128     C                   EVAL      I95NAR = VABNZD
150000040128     C                   EVAL      I95CAP = VABCAD
150100040128     C                   EVAL      I95LOC = VABLOD
150200040128     C                   CALL      'TISI95R'
150300040128     C                   PARM                    TISI95DS
150400040128     C                   EVAL      VABPRD = O95PRV
150500040510     C                   ENDIF
150600020204     C*
150700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
150800020204     C                   EXSR      CHKIMPDIV
150900030715     C                   ENDIF
151000020204     C*
151100010607     C                   ENDSR
151200010607     C*----------------------------------------------------*
151300010601
151400020204
151500020204     C*----------------------------------------------------*
151600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
151700020204     C*----------------------------------------------------*
151800020204     C     CHKIMPDIV     BEGSR
151900020204     C*
152000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152100020204     C                   Z-ADD     *zeros        wrkDec            9 9
152200020204     C*
152300020204     C* Come prima cosa effettuo considerazioni sulla divisa
152400020204     C                   IF        vabIAS > *zeros
152500020204     C                   IF        vabVAS <> 'EUR'
152600020204     C                   EVAL      vabVAS =  'ITL'
152700020204     C                   ENDIF
152800020204     C                   ENDIF
152900020204     C*
153000020204     C                   IF        vabCAS > *zeros
153100020204     C                   IF        vabVCA <> 'EUR'
153200020204     C                   EVAL      vabVCA =  'ITL'
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C                   IF        vabVMD > *zeros
153700020204     C                   IF        vabVAD <> 'EUR'
153800020204     C                   EVAL      vabVAD =  'ITL'
153900020204     C                   ENDIF
154000020204     C                   ENDIF
154100020204     C*
154200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154300020204     C                   Z-ADD     vabIAS        wrkDec
154400020204     C                   IF        wrkDec > *zeros
154500020204     C                   IF        vabVAS = 'ITL'
154600020204     C                   EVAL      vabIAS = *zeros
154700020204     C                   ENDIF
154800020204     C                   ENDIF
154900020204     C*
155000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155100020204     C                   Z-ADD     vabCAS        wrkDec
155200020204     C                   IF        wrkDec > *zeros
155300020204     C                   IF        vabVCA = 'ITL'
155400020204     C                   EVAL      vabCAS = *zeros
155500020204     C                   ENDIF
155600020204     C                   ENDIF
155700020204     C*
155800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
155900020204     C                   Z-ADD     vabVMD        wrkDec
156000020204     C                   IF        wrkDec > *zeros
156100020204     C                   IF        vabVAD = 'ITL'
156200020204     C                   EVAL      vabVMD = *zeros
156300020204     C                   ENDIF
156400020204     C                   ENDIF
156500020204     C*
156600020204     C                   ENDSR
156700020204     C***
156800010330
156900100318
157000100318
157100100318     C*----------------------------------------------------*
157200100318     C*  CONTROLLO NUMERICITA' CAMPI
157300100318     C*----------------------------------------------------*
157400100318     C     CHKNUM        BEGSR
157500100318     C*
157600100318     C                   IF        PiDecChr = *blanks
157700100318     C                   EVAL      PiDecChr = CharNUM
157800100318     C                   ENDIF
157900100318     C*
158000100318     C                   callp     UBISNUM_Check(PiStr
158100100318     C                                          :PiDecChr
158200100318     C                                          :PiVal
158300100318     C                                          :PiNum
158400100318     C                                          :PiInt)
158500100318     C*
158600100318     C                   ENDSR
158700100318     C***
158800010601
158900010601
159000010601
159100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159200010601     C     repfil        BEGSR
159300010601     C*
159400010601     C                   if        invfil = *zeros and
159500010601     C                             depfil > *zeros and
159600010629     C                             (vinflg = *blanks or
159700010629     C                              vinflg = *zeros)
159800010601     C
159900010601     C                   eval      invfil = depfil
160000010601     C                   endif
160100010601     C*
160200010601     C                   if        depfil <> invfil and
160300010601     C                             invfil > *zeros
160400010601     C                   eval      flgMulti = '1'
160500010601     C                   if        vinflg = *blanks
160600010601     C                   add       1             cntNonEl
160700010601     C                   endif
160800010601     C                   endif
160900010601     C*
161000010601     C                   if        vinflg = '2'
161100010601     C                   eval      flgStato = '2'
161200010601     C                   endif
161300010601     C*
161400010601     C                   ENDSR
161500010601     C***
161600010601
161700010601
161800010601
161900010330
162000040119      /TITLE Invio dei dati al punto operativo.
162100040119     C     invio         BEGSR
162200040119     C*
162300040119     C* 1° invio FIVAT
162400040119     C                   reset                   dscmz
162500040119     C                   move      vlrpoi        cmzdst
162600040119     C                   eval      cmzfld = 'FIVATWWR'
162700040119     C                   eval      cmzmbd = vlrhdl
162800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162900040119     C***                if        prmfir = *blanks
163000040119     C                   eval      cmzfla = 'FIVAT00F'
163100040119     C                   eval      cmzmba = 'FIVAT00F'
163200040119     C***                else
163300040119     C***                eval      cmzfla = prmfir
163400040119     C***                eval      cmzmba = prmfir
163500040119     C***                endif
163600040119     C                   eval      cmznrr = *zeros
163700040119     C                   move      §ctrokvt      cmznrr
163800040119     C                   eval      cmzlba = vlrfl1
163900040119     C                   call(e)   'TIS711C'
164000040119     C                   parm                    dscmz
164100040119     C                   parm      *blanks       esito
164200040119     C                   if        %error
164300040119     C                             or cmzerr = '1'
164400040119     C                             or esito  = '1'
164500040119     C                   eval      wrkesito = '3'
164600040119     C                   else
164700040119     C*
164800040119     C* 2° invio FIVAB
164900040119     C                   reset                   dscmz
165000040119     C                   move      vlrpoi        cmzdst
165100040119     C                   eval      cmzfld = vlrfou
165200040119     C                   eval      cmzmbd = vlrhdl
165300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165400040119     C***                if        prmfir = *blanks
165500040119     C                   eval      cmzfla = 'FIVAB00F'
165600040119     C                   eval      cmzmba = 'FIVAB00F'
165700040119     C***                else
165800040119     C***                eval      cmzfla = prmfir
165900040119     C***                eval      cmzmba = prmfir
166000040119     C***                endif
166100040119     C                   eval      cmznrr = *zeros
166200040119     C                   move      §ctrokvb      cmznrr
166300040119     C                   eval      cmzlba = vlrfl1
166400040119     C                   call(e)   'TIS711C'
166500040119     C                   parm                    dscmz
166600040119     C                   parm      *blanks       esito
166700040119     C                   if        %error
166800040119     C                             or cmzerr = '1'
166900040119     C                             or esito  = '1'
167000040119     C                   eval      wrkesito = '3'
167100040119     C                   endif
167200040119     C                   endif
167300040119     C*
167400040119     C                   ENDSR
167500040119     C***
167600010601
167700010601
167800010601
167900010601
168000010601      /TITLE Invio dei dati al punto operativo.
168100010601     C     opeini        BEGSR
168200010601     C*
168300010601     C* Inizializzo flag e contatori operativi
168400010601     C                   movel     '0'           flgGiro           1
168500010601     C                   movel     '0'           flgMulti          1
168600010601     C                   movel     '1'           flgStato          1
168700010615     C                   movel     '0'           flgOk             1
168800010601     C                   z-add     *zeros        cntNonEl         10 0
168900010601     C                   z-add     *zeros        depfil            3 0
169000010601     C                   z-add     *zeros        invfil            3 0
169100010601     C*
169200010601     C                   ENDSR
169300010601     C***
169400070326
169500070326
169600070326
169700070326
169800070326     C     *pssr         BEGSR
169900070329     C*
170000070329     C                   if        %open(tivin00r)
170100070329     C                   close     tivin00r
170200070329     C                   endif
170300070329     C                   if        %open(fivabwwr)
170400070329     C                   close     fivabwwr
170500070329     C                   endif
170600070329     C                   if        %open(fivatwwr)
170700070329     C                   close     fivatwwr
170800070329     C                   endif
170900070326     C*
171000070326     C* Effettuo la chiamata al CLLE preposto
171100070326     C                   call(e)   'TITVVTC'
171200070326     C                   parm                    parccm
171300070326     C                   parm                    parmbr
171400070326     C                   parm      '2'           paropz
171500070326     C*
171600070326     C                   eval      wrkesito = '2'
171700070404     C*
171800070404     C                   seton                                        LR
171900070326     C*
172000070326     C                   ENDSR     '*CANCL'
172100070326     C***
172200100318
172300100318
172400100318
172500100318     C     normCSV       BEGSR
172600100318     C*
172700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
172800100318     C* separatori campi
172900100318     C                   if        CharTXT <> *blanks
173000100318     C                   z-add     *zeros        wLenVINDTA        4 0
173100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173200100318     C                   setoff                                       80
173300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173600100318     C                   if        not *in80
173700100318     C                   seton                                        80
173800100318     C                   else
173900100318     C                   setoff                                       80
174000100318     C                   endif
174100100318     C                   endif
174200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174300100318     C                             and *in80 = *on
174400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174500100318     C                   endif
174600100318     C                   enddo
174700100318     C                   endif
174800100318     C*
174900100318     C                   ENDSR
175000100318     C***
175100070326
175200100326
175300100326
175400100326     C*----------------------------------------------------*
175500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175600100326     C*----------------------------------------------------*
175700100326     C     REPNSP        BEGSR
175800100326     C*
175900100326     C* NSP => Stacco un numeratore da AZNUM
176000100326     C                   clear                   TRUL33DS
176100100326     C                   eval      I33OPE = *zeros
176200100326     C                   eval      I33CNU = 302
176300100326     C                   eval      I33NUM = 1
176400100326     C                   movel     TRUL33DS      KPJBU
176500100326     C                   call      'TRUL33R'
176600100326     C                   parm                    KPJBA
176700100326     C                   movel     KPJBU         TRUL33DS
176800100326     C                   if        O33ERR = *zeros
176900100326     C                   z-add     O33NRF        VABNSP
177000100326     C                   z-add     O33NRF        VATNSP
177100100326     C                   else
177200100326     C                   SETON                                        31
177300100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177400100326     C                             + ' ' + 'VABNSP VATNSP'
177500100326     C                   endif
177600100326     C*
177700100326     C                   ENDSR
177800010330
177900100326
178000010330
178100000613     C     *inzsr        BEGSR
178200990910     C*
178300990910     C     *entry        plist
178400990920     C                   parm                    tivlrds
178500990921     C                   parm      wrkesito      esito
178600000724     C                   parm                    prmlit
178700000710     C                   parm                    prmfir
178800010330     C*
178900010330     C* CALCOLA LA DATA CORRENTE
179000100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179100100318     C                   eval      datcor = %dec(%date() : *ISO)
179200000613     C*
179300000613     C                   ENDSR
179400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179500140326W_VABRSD;W_VABRD2;VABLOD;VABIND;VABCAD;VABPRD;VABNCL;VABPKB;VABNOT;X;
179600130222VABIAS;W_VABRMA;VATNOT_B;VABNAS;VABRMO;
179700120604
179800120604
179900120604
