000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120604      *
000300120604      * PARTICOLARITA':
000400130415     C*** VABRMN sono i caratteri dal 4 x 4 di RMA
000500130415     C*** oppure, se RMA vuoto, sono gli ultimi 6 char del barcode
000600130404     C*** se RMA è vuoto, uso il barcode
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
004200130122     D Intestazioni    s               n   inz(*on)
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
007000130222     D w70             s             70
007100130222     D wRSD            s             35
007200130222     D wRD2            s             35
007300120411     D Data8A          s              8
007400130404     D wLast6          s              3s 0
007500030822     D*------------------
007600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007700030822     D*------------------
007800030822     D posDaDft        S              3  0 INZ(*zeros)
007900030822     D posADft         S              3  0 INZ(*zeros)
008000030822     D j               s              3  0 INZ(1)
008100030822     D wGiroDft        s              1  0 INZ(*zeros)
008200080418     D*------------------
008300080418     D* Costanti
008400080418     D*------------------
008500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008700100318
008800100318     D*------------------
008900100318     D* LINKING A DEFINIZIONI ESTERNE
009000100318     D*------------------
009100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
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
021300010601     C*
021400070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021500070925     C                   eval      vindta  = ' ' + %trim(vindta)
021600070925     C*
021700080527     C                   exsr      inzvar
021800100318     C                   exsr      normCSV
021900010605     C                   exsr      impvab
022000010601     C*
022100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200010605     C                   eval      depfil = VABLNP
022300010601     C                   exsr      repfil
022400010601     C                   if        depfil = invfil
022500021025     C                   if        vlrpoi = 999
022600030822     C                   move(P)   invfil        VABFGS
022700021025     C                   else
022800030822     C                   move(P)   vlrpoi        VABFGS
022900021025     C                   endif
023000010601     C*
023100010601     C                   exsr      PREELA
023200010601     C*
023300010604     C* Ebbene...
023400010604     C*
023500040119     C  N31              add       1             §CTROKVB
023600040119     C   32              add       1             §CTRMO
023700040119     C   31              add       1             §CTRNO
023800080418     C*
023900030822     C                   if        wGiro = 2
024000080513     C* Scrivo sempre il VAT
024100080513     C  N31              exsr      wrivat
024200110201     C                   exsr      chkvab
024300080513     C                   endif
024400010604     C*
024500010604     C                   if        *in31 = *off and
024600010604     C                             *in32 = *off
024700010604     C                   eval      vinflg = '1'
024800010604     C                   else
024900010604     C                   eval      vinflg = '2'
025000010604     C                   endif
025100010604     C                   endif
025200010601     C*
025300010604     C                   endif
025400010604     C*
025500010330     C                   else
025600010330     C                   eval      vinflg = '1'
025700010330     C                   endif
025800010601     C*
025900010601     C  N70              update    tivin000
026000010330     C*
026100030822     C  N70              enddo
026200040119     C*
026300080527     C* Verifico dati rimasti "in cannna"...
026400080527     C                   seton                                        65
026500110201     C                   exsr      chkvab
026600080527     C*
026700040119     C                   endif                                                  (endif - chkcall)
026800010601     C*
026900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027000010601     C                   if        cntNonEl = *zeros or
027100010601     C                             flgMulti = '0'
027200010330     C* Se non ci sono record con errori ...
027300010601     C                   if        §ctrno = 0 and
027400010604     C                             §ctrmo = 0 and
027500010601     C                             flgStato <> '2'
027600010330     C* ... restituisco esito OK.
027700010330     C                   eval      wrkesito = '0'
027800010330     C                   else
027900040119     C                   if        §ctrokvb > 0
028000010330     C                   eval      wrkesito = '1'
028100010330     C                   else
028200010615     C                   if        flgOk = '0'
028300010615     C                   eval      wrkesito = '2'
028400010615     C                   else
028500010615     C                   eval      wrkesito = '6'
028600010615     C                   endif
028700010330     C                   endif
028800010330     C                   endif
028900010601     C                   else
029000010601     C                   eval      wrkesito = '9'
029100010601     C                   endif
029200010330     C*
029300010330     C                   if        %open(tivin00r)
029400010330     C                   close     tivin00r
029500010330     C                   endif
029600021025     C                   if        %open(fivabwwr)
029700021025     C                   close     fivabwwr
029800010330     C                   endif
029900040512     C                   if        %open(fivatwwr)
030000040512     C                   close     fivatwwr
030100040512     C                   endif
030200010601     C*
030300010601     C                   if        vlrpoi <> 999
030400010601     C                   eval      invfil = vlrpoi
030500010601     C                   endif
030600010330     C*
030700040119     C                   if        §ctrokvb > 0
030800010601     C                             and invfil > *zeros
030900010330     C                   exsr      invio
031000010330     C                   endif
031100010601     C*
031200010618     C                   if        flgGiro = '1'
031300010601     C                   exsr      endela
031400010618     C                   endif
031500010330     C*
031600010330     C                   ENDSR
031700010330     C***
031800080527
031900080527
032000080527
032100080527     C*----------------------------------------------------*
032200080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032300080527     C*----------------------------------------------------*
032400100413     C     CHKVAB        BEGSR
032500110201     C*
032600110201     C* Se richiesti ignorare colli / peso / volume
032700110315 xxx C***                if        *in35
032800110315 xxx C***                z-add     *zeros        VABNCL
032900110315 xxx C***                z-add     *zeros        VABPKB
033000110315 xxx C***                z-add     *zeros        VABVLB
033100110315 xxx C***                endif
033200080527     C*
033300080527     C* Scrivo il VAB solo a rottura d spedizione
033400080527     C                   if        depspe = *zeros
033500080527     C  N31              eval      fivabds_sav = fivabds
033600080527     C                   eval      depspe = vabNSP
033700080527     C                   else
033800080527     C                   if        depspe = vabNSP AND *in65 = *off
033900120703     C   51              add       VABNCL        sav_VABNCL
034000100413     C   51              add       VABPKB        sav_VABPKB
034100100413     C   51              add       VABVLB        sav_VABVLB
034200100720     C   52              z-add     VABNCL        sav_VABNCL
034300100720     C   52              z-add     VABPKB        sav_VABPKB
034400100720     C   52              z-add     VABVLB        sav_VABVLB
034500100720     C   53              add       VABNCL        sav_VABNCL
034600100720     C   53              z-add     VABPKB        sav_VABPKB
034700100720     C   53              z-add     VABVLB        sav_VABVLB
034800080527     C                   else
034900080527     C  N31              eval      fivabds_ok = fivabds_sav
035000080527     C  N31              exsr      wrivab
035100080527     C  N31              eval      fivabds_sav = fivabds
035200080527     C                   eval      depspe = vabNSP
035300080527     C                   endif
035400080527     C                   endif
035500080527     C*
035600080527     C                   ENDSR
035700040119
035800040119
035900040119
036000040119     C*----------------------------------------------------*
036100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036200040119     C*----------------------------------------------------*
036300040510     C     WRIVAB        BEGSR
036400080513     C*
036500080513     C* Gestisco l'eventuale rottura x numero spedizione
036600080527     C                   if        VABNSP <> depspe or *in65 = *on
036700070301     C                   if        wVATNOT_E <> *blanks
036800070301     C                   eval      VABCTM = '7Q'
036900070301     C                   endif
037000100413     C                   write     fivab000
037100080513     C                   endif
037200040119     C*
037300040119     C                   ENDSR
037400040119
037500040119
037600040119
037700040119     C*----------------------------------------------------*
037800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037900040119     C*----------------------------------------------------*
038000040510     C     WRIVAT        BEGSR
038100060512     C*
038200060512     C* Inizializzo i campi chiave
038300060512     C                   eval      VATFGS = VABFGS
038400060512     C                   eval      VATAAS = VABAAS
038500060512     C                   eval      VATCCM = VABCCM
038600070823     C                   eval      VATNRS = VABNRS
038700060512     C                   eval      VATNSP = VABNSP
038800060512     C                   eval      VATLNP = VABLNP
038900040119     C*
039000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039100040510     C                   if        VATTRC = *blanks
039200040510     C                   eval      VATTRC = 'E'
039300040510     C                   endif
039400110318     C                   if        VATNOT <> *blanks
039500040510     C                   write     FIVAT000
039600040119     C                   add       1             §CTROKVT
039700110318     C                   endif
039800060331     C*
039900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040000060331     C                   if        wVATNOT_A <> *blanks
040100060331     C                   eval      VATNOT = wVATNOT_A
040200060331     C                   eval      VATTRC = 'A'
040300060331     C                   write     FIVAT000
040400060331     C                   add       1             §CTROKVT
040500060331     C                   endif
040600060331     C*
040700060331     C                   if        wVATNOT_B <> *blanks
040800060331     C                   eval      VATNOT = wVATNOT_B
040900060331     C                   eval      VATTRC = 'B'
041000060331     C                   write     FIVAT000
041100060331     C                   add       1             §CTROKVT
041200060331     C                   endif
041300060331     C*
041400060331     C                   if        wVATNOT_E <> *blanks
041500060331     C                   eval      VATTRC = 'E'
041600120629     C*
041700130320     C* NON sviluppo i "CHI SONO" perché spedizione monocollo
041800120703     C*
041900120711     C                   EVAL      VATNOT = wVATNOT_E
042000120629     C                   write     FIVAT000
042100120629     C                   add       1             §CTROKVT
042200120604     C*
042300120604     C                   endif
042400110927     C*
042500110927     C                   if        wVATNOT_I <> *blanks
042600110927     C                   eval      VATNOT = wVATNOT_I
042700110927     C                   eval      VATTRC = 'I'
042800110927     C                   write     FIVAT000
042900110927     C                   add       1             §CTROKVT
043000110927     C                   endif
043100040119     C*
043200040119     C                   ENDSR
043300040119
043400040119
043500040119
043600040119     C*----------------------------------------------------*
043700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043800040119     C*----------------------------------------------------*
043900040119     C     PREVAT        BEGSR
044000040119     C*
044100040119     C* Compongo il nome del membro da dare al FIVATWWR
044200040119     C                   eval      parmbr = vlrhdl
044300040119     C                   movel     'M'           parmbr
044400040510     C                   eval      parccm = vlrksc
044500040119     C                   eval      paropz = '1'
044600040119     C* Effettuo la chiamata al CLLE preposto
044700040119     C                   call(e)   'TITVVTC'
044800040119     C                   parm                    parccm
044900040119     C                   parm                    parmbr
045000040119     C                   parm                    paropz
045100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045200040119     C                   if        %error
045300040119     C                   movel     '1'           chkcall
045400040119     C                   else
045500040119     C                   movel     '0'           chkcall
045600040119     C                   endif
045700040119     C*
045800040119     C                   ENDSR
045900010601
046000010601
046100010601
046200010330     C*----------------------------------------------------*
046300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046400010330     C*----------------------------------------------------*
046500040119     C     INZVAR        BEGSR
046600080527     C*
046700080527     C                   setoff                                       65
046800010330     C*
046900030822     C* Inizializzo variabili di wrk
047000020204     C                   Z-ADD     *zeros        Num5_0            5 0
047100101018     C                   MOVEL     *blanks       wFlgCAS
047200060331     C                   MOVEL     *blanks       wVATNOT_A
047300060331     C                   MOVEL     *blanks       wVATNOT_B
047400060331     C                   MOVEL     *blanks       wVATNOT_E
047500110927     C                   MOVEL     *blanks       wVATNOT_I
047600120411     C                   MOVEL     *blanks       wNOT
047700120411     C                   MOVEL     *blanks       Data8A
047800030822     C*
047900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048000030822     C                   CLEAR                   SkSplitCSV
048100030822     C                   EVAL      i = 1
048200030822     C                   EVAL      posDa = *zeros
048300030822     C                   EVAL      posA  = *zeros
048400080527     C*
048500080527     C                   CLEAR                   FIVABDS
048600080527     C                   CLEAR                   FIVAT000
048700030822     C*
048800030822     C* Reimposto i valori di default
048900030822     C                   EXSR      DEFCAM
049000030822     C*
049100010330     C                   ENDSR
049200010330     C*----------------------------------------------------*
049300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049400010330     C*----------------------------------------------------*
049500010330     C     DEFCAM        BEGSR
049600030822     C*
049700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049800030715     C* e delimitatore testo.
049900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050200100318     C*
050300100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050400100318     C                   EVAL      CharSOS = CharNUM
050500030822     C*
050600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050700100720     C                   SETOFF                                       505152
050800100720     C                   SETOFF                                       53
050900100413     C                   SELECT
051000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051100100720     C                   SETON                                        50
051200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051300100413     C                   SETON                                        51
051400100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051500100720     C                   SETON                                        52
051600100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051700100720     C                   SETON                                        53
051800100413     C                   ENDSL
051900030822     C*
052000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052100030822     C                   EVAL      posDaDft = 1
052200030822     C                   EVAL      posADft  = 0
052300030822     C                   EVAL      wGiroDft = 0
052400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052500030822     C                             posDaDft > 0
052600030822     C*
052700030822     C* Gestisco il 1° giro
052800030822     C                   IF        wGiroDft = 0
052900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053100030822     C* Incremento il contatore dei "giri"
053200030822     C                   EVAL      wGiroDft = 1
053300030822     C                   ELSE
053400030822     C                   EVAL      posDaDft = posADft
053500030822     C                   ENDIF
053600030822     C* Eseguo lo scan x trovare la fine del campo corrente
053700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053800030822     C*
053900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054100030822     C                   IF        posDaDft > 0 AND
054200030822     C                             posADft  > 0
054300030822     C* NCL
054400030822     C                   IF        %subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):1:3)
054700030822     C                             = 'NCL'
054800030822     C                   EVAL      PiStr=%trim(%subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100030822     C                   EXSR      CHKNUM
055200030822     C                   IF        PiInt=*on
055300030822     C                   Z-ADD     PiVal         VABNCL
055400030822     C                   ENDIF
055500030822     C                   ENDIF
055600030822     C* CCM
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'CCM'
056100030822     C                   EVAL      PiStr=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   EXSR      CHKNUM
056500030822     C                   IF        PiInt=*on
056600030822     C                   Z-ADD     PiVal         VABCCM
056700030822     C                   ENDIF
056800030822     C                   ENDIF
056900030822     C* LNP
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'LNP'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900030822     C                   Z-ADD     PiVal         VABLNP
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* NRS
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'NRS'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiInt=*on
059200030822     C                   Z-ADD     PiVal         VABNRS
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* CTR
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'CTR'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500030822     C                   Z-ADD     PiVal         VABCTR
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* PKB
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'PKB'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiNum=*on
061800120418     C                   Z-ADD     PiVal         VABPKB
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* VLB
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'VLB'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiNum=*on
063100030822     C                   Z-ADD     PiVal         VABVLB
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* QFT
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'QFT'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiNum=*on
064400030822     C                   Z-ADD     PiVal         VABQFT
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* CBO
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'CBO'
065200030822     C                   EVAL      VABCBO=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   ENDIF
065600030822     C* TSP
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'TSP'
066100030822     C                   EVAL      VABTSP=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* VAS
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'VAS'
067000030822     C                   EVAL      VABVAS=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400030822     C* VCA
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'VCA'
067900030822     C                   EVAL      VABVCA=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300070925     C* TIC
068400070925     C                   IF        %subst(
068500070925     C                             %subst(vlrppt:posDaDft+1:
068600070925     C                             posADft-posDaDft-1):1:3)
068700070925     C                             = 'TIC'
068800070925     C                   EVAL      VABTIC=%trim(%subst(
068900070925     C                             %subst(vlrppt:posDaDft+1:
069000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100070925     C                   ENDIF
069200030822     C* GCA
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'GCA'
069700030822     C                   EVAL      VABGCA=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100030822     C* CTM
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'CTM'
070600030822     C                   EVAL      VABCTM=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* FFD
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'FFD'
071500030822     C                   EVAL      VABFFD=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* VAD
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'VAD'
072400030822     C                   EVAL      VABVAD=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* GMA
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'GMA'
073300030822     C                   EVAL      VABGMA=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* GGA
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'GGA'
074200030822     C                   EVAL      VABGGA=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* GVA
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'GVA'
075100030822     C                   EVAL      VABGVA=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* TC1
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'TC1'
076000030822     C                   EVAL      VABTC1=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* TC2
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'TC2'
076900030822     C                   EVAL      VABTC2=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300040714     C* VATTRC
077400040714     C                   IF        %subst(
077500040714     C                             %subst(vlrppt:posDaDft+1:
077600040714     C                             posADft-posDaDft-1):1:3)
077700040714     C                             = 'TRC'
077800040714     C                   EVAL      VATTRC=%trim(%subst(
077900040714     C                             %subst(vlrppt:posDaDft+1:
078000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100040714     C                   ENDIF
078200030822     C* ...
078300030822     C                   ENDIF
078400030822     C                   ENDDO
078500020204     C*
078600010330     C                   ENDSR
078700010607     C*----------------------------------------------------*
078800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078900010607     C*----------------------------------------------------*
079000010607     C     IMPVAB        BEGSR
079100010607     C*
079200010607     C                   SETOFF                                       3132
079300110315 xxx C***                SETON                                        35
079400030515     C*
079500010607     C                   MOVEL     datcor        VABAAS
079600010607     C                   MOVE      datcor        VABMGS
079700010607     C*
079800030715     C**********
079900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080000030715     C**********
080100030715     C                   IF        wGiro = *zeros
080200040113     C*
080300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080600080429     C                             %trim(intcol(5))<>*blanks
080700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
080900080429     C                                      %trim(intcol(5))
081000070823     C     ';':CharCSV   XLATE     vindta        vindta
081100040510     C                   ENDIF
081200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081300031201     C     minu:maiu     XLATE     vindta        vindta
081400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081500030715     C                   DOW       posDa <= %len(%trim(vindta))
081600030715     C*
081700030715     C* Gestisco il 1° campo
081800030715     C                   IF        i = 1
081900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082000030715     C                   EVAL      posDa = 1
082100030715     C* Eseguo lo scan x trovare la fine del primo campo
082200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082300030715     C* A questo punto "estraggo" il campo corrente
082400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082500030715     C                                                    (posA-posDa))
082600030715     C* X i campi successivi al 1°
082700030715     C                   ELSE
082800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082900030715     C                   EVAL      posDa = posA + 1
083000030715     C* Eseguo lo scan x trovare la fine del campo corrente
083100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083200030715     C* Gestisco l'ultimo campo
083300030715     C                   IF        posA = *zeros
083400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083500030715     C* A questo punto "estraggo" il campo corrente
083600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083700030715     C                                                    (posA-posDa))
083800030715     C                   LEAVE
083900030715     C                   ELSE
084000030715     C* A questo punto "estraggo" il campo corrente
084100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084200030715     C                                                    (posA-posDa))
084300030715     C                   ENDIF
084400030715     C                   ENDIF
084500030715     C* Incremento il contatore d campo
084600030715     C                   EVAL      i = i +1
084700030715     C                   ENDDO
084800030715     C*
084900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085000030715     C* delimitazione testo
085100030715     C                   EVAL      i = 1
085200030715     C                   DOW       i < %elem(SkSplitFLD)
085300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085500030715     C                   EVAL      i = i + 1
085600030715     C                   ENDDO
085700030715     C                   EVAL      wGiro = 1
085800030715     C                   ELSE
085900030715     C                   EVAL      wGiro = 2
086000031204     C**********
086100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086200031204     C* d separatore campo
086300031204     C**********
086400031204     C                   EVAL      vindta = %trim(vindta)
086500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086600031204     C                   EVAL      vindta = %subst(vindta:2)
086700031204     C                   ENDDO
086800031204     C*
086900031204     C                   Z-ADD     *zeros        lunghInput        4 0
087000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087400110912     C                   ENDDO
087500030715     C**********
087600030715     C* Effettuo lo split del campo dati d input
087700030715     C**********
087800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087900030715     C                   DOW       posDa <= %len(%trim(vindta))
088000030715     C*
088100030715     C* Gestisco il 1° campo
088200030715     C                   IF        i = 1
088300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088400030715     C                   EVAL      posDa = 1
088500030715     C* Eseguo lo scan x trovare la fine del primo campo
088600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088700030715     C* A questo punto "estraggo" il campo corrente
088800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088900030715     C                                                    (posA-posDa))
089000030715     C* X i campi successivi al 1°
089100030715     C                   ELSE
089200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089300030715     C                   EVAL      posDa = posA + 1
089400031204     C* Verifico che nn vi sia il campo nn valorizzato
089500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089600031204     C* Se campo nn valorizzato skippo al prossimo
089700031204     C                   EVAL      SkSplitCSV(i) = *blanks
089800031204     C                   EVAL      posA  = posA + 1
089900031204     C                   ELSE
090000030715     C* Eseguo lo scan x trovare la fine del campo corrente
090100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090200030715     C* Gestisco l'ultimo campo
090300030715     C                   IF        posA = *zeros
090400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090500030715     C* A questo punto "estraggo" il campo corrente
090600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090700030715     C                                                    (posA-posDa))
090800030715     C                   LEAVE
090900030715     C                   ELSE
091000030715     C* A questo punto "estraggo" il campo corrente
091100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091200030715     C                                                    (posA-posDa))
091300030715     C                   ENDIF
091400031204     C                   ENDIF
091500030715     C                   ENDIF
091600030715     C* Incremento il contatore d campo
091700030715     C                   EVAL      i = i +1
091800030715     C                   ENDDO
091900030715     C*
092000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092100030715     C* delimitazione testo
092200030715     C                   EVAL      i = 1
092300030715     C                   DOW       i < %elem(SkSplitCSV)
092400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092600030715     C                   EVAL      i = i + 1
092700030715     C                   ENDDO
092800100720     C*
092900130222     C* Stacco un progressivo x numero spedizione
093000130222     C                   EXSR      repNSP
093100100326     C*
093200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093300030715     C                   EVAL      i = 1
093400030715     C                   DOW       i < %elem(SkSplitFLD)
093500030715     C*
093600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093800030715     C                             SkSplitCSV(i) <> *blanks
093900030721     C*** ATB
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094100030721     C                   EVAL      VABATB = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030715     C*** RSD
094400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094800030715     C* ==
094900030715     C                   ENDIF
095000030721     C*** RD2
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030715     C*** IND
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095600030715     C                   EVAL      VABIND = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** LOD
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** PRD
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030721     C*** NZD
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030715     C*** RMA
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** NOT
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** NT2
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** VCA
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** VAS
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** FFD
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030721     C*** GC1
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** GC2
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** TSP
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100500030721     C                   ENDIF
100600040510     C*** CBO
100700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100900040510     C                   ENDIF
101000030721     C*** NAS
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** TIC
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** GCA
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** XCO
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** CTM
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** TCR
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** CTS
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** FTM
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** VAD
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** GMA
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GGA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** GVA
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** TC1
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** TC2
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** SCL
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** RMO
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** NMO
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*
107900030721     C* Reperisco quindi i campi numerici...
108000040510     C*** NSP
108100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108200040510     C                   EVAL      PiStr=SkSplitCSV(i)
108300040510     C                   EXSR      CHKNUM
108400040510     C                   IF        PiInt=*on
108500040510     C                   Z-ADD     PiVal         VABNSP
108600040510     C                   ELSE
108700040510     C                   SETON                                        31
108800040510     C                   EVAL      VABNSP = *zeros
108900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109000040510     C                             + ' ' + 'VABNSP'
109100040510     C                   ENDIF
109200040510     C                   ENDIF
109300071106     C*** RMN
109400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109500071106     C                   EVAL      PiStr=SkSplitCSV(i)
109600071106     C                   EXSR      CHKNUM
109700071106     C                   IF        PiInt=*on
109800071106     C                   Z-ADD     PiVal         VABRMN
109900071106     C                   ELSE
110000071106     C                   SETON                                        32
110100071106     C                   EVAL      VABRMN = *zeros
110200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110300071106     C                             + ' ' + 'VABRMN'
110400071106     C                   ENDIF
110500071106     C                   ENDIF
110600030721     C*** CAD
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiInt=*on
111100030721     C                   Z-ADD     PiVal         Num5_0
111200030721     C                   MOVEL(p)  Num5_0        VABCAD
111300030721     C                   ELSE
111400030721     C                   SETON                                        32
111500030721     C                   EVAL      VABCAD = *zeros
111600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111700030721     C                             + ' ' + 'VABCAD'
111800030721     C                   ENDIF
111900030721     C                   ENDIF
112000030721     C*** NCL
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112200030721     C                   EVAL      PiStr=SkSplitCSV(i)
112300030721     C                   EXSR      CHKNUM
112400030721     C                   IF        PiInt=*on
112500030721     C                   Z-ADD     PiVal         VABNCL
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      VABNCL = *zeros
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000040122     C                             + ' ' + 'VABNCL'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** PKB
113400120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
113500120418     C* per cui si arrotonda il valore passato agli etti
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiNum=*on
114000120418     C                   Z-ADD(H)  PiVal         VABPKB
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
115200120713     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
115300120713     C                             %trim(SkSplitCSV(i)) <> '0,00'
115400120605     C                   EVAL      VABVCA = 'EUR'
115500101018     C                   EVAL      wFlgCAS = '1'
115600101018     C                   ENDIF
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiNum=*on
116000030721     C                   Z-ADD     PiVal         VABCAS
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABCAS = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABCAS'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** CCM
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on      AND
117300030721     C                             PiVal<=9999999 AND
117400030721     C                             PiVal>*zeros
117500030721     C                   Z-ADD     PiVal         VABCCM
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABCCM = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000040510     C                             + ' ' + 'VABCCM'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** LNP
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiInt=*on    AND
118800030721     C                             PiVal<=999   AND
118900030721     C                             PiVal>*zeros
119000030721     C                   Z-ADD     PiVal         VABLNP
119100030721     C                   ELSE
119200040715     C                   SETON                                        32
119300030721     C                   EVAL      VABLNP = *zeros
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500040510     C                             + ' ' + 'VABLNP'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** NRS
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on AND
120300030721     C                             PiVal<=99
120400030721     C                   Z-ADD     PiVal         VABNRS
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABNRS'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** LNA
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on    AND
121600030721     C                             PiVal<=999   AND
121700030721     C                             PiVal>*zeros
121800030721     C                   Z-ADD     PiVal         VABLNA
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABLNA = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABLNA'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** CTR
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on  AND
123100030721     C                             PiVal<=999
123200030721     C                   Z-ADD     PiVal         VABCTR
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABCTR'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** IAS
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiNum=*on
124400030721     C                   Z-ADD     PiVal         VABIAS
124500130222     C                   EVAL      vabVAS = 'EUR'
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABIAS'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** VLB
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABVLB
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABVLB'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** QFT
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABQFT
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABQFT'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** NCD
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on      AND
128100030721     C                             PiVal<=9999999
128200030721     C                   Z-ADD     PiVal         VABNCD
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABNCD'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** NCA
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on      AND
129400030721     C                             PiVal<=9999999
129500030721     C                   Z-ADD     PiVal         VABNCA
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABNCA'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** ZNC
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on AND
130700030721     C                             PiVal<=99
130800030721     C                   Z-ADD     PiVal         VABZNC
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABZNC'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** DCR
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on       AND
132000030721     C                             PiVal<=99999999
132100030721     C                   Z-ADD     PiVal         VABDCR
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABDCR'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** HCR
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on   AND
133300030721     C                             PiVal<=9999
133400030721     C                   Z-ADD     PiVal         VABHCR
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABHCR'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** VMD
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiNum=*on
134600030721     C                   Z-ADD     PiVal         VABVMD
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABVMD'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** ANT
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on        AND
135800030721     C                             PiVal<=999999999
135900030721     C                   Z-ADD     PiVal         VABANT
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABANT'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** CMO
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on
137100030721     C                   Z-ADD     PiVal         Num5_0
137200030721     C                   MOVEL(p)  Num5_0        VABCMO
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      VABCMO = *zeros
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABCMO'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000040510     C*
138100040510     C* Reperisco i campi del FIVAT
138200040510     C*** VATNOT
138300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138500040510     C                   ENDIF
138600040510     C*** VATTRC
138700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138900040510     C                   ENDIF
139000060331     C*** VATNOT_A
139100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139300060331     C                   ENDIF
139400060331     C*** VATNOT_B
139500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139700060331     C                   ENDIF
139800060331     C*** VATNOT_E
139900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140100060331     C                   ENDIF
140200110927     C*** VATNOT_I
140300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140500110927     C                   ENDIF
140600070823     C*
140700120713 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
140800120713
140900120301
141000070925     C*
141100071029     C***  <===  -----------------------------------------
141200040510     C*
141300030715     C                   ENDIF
141400030715     C                   EVAL      i = i + 1
141500030715     C                   ENDDO
141600070925     C*
141700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
141800120411     C*
141900130415     C*** VABRMN sono i caratteri dal 4 x 4 di RMA
142000130415     C*** oppure, se RMA vuoto, sono gli ultimi 6 char del barcode
142100130415     C                   IF        VABRMA <> *blank
142200130415     C                   EVAL      VABRMN =
142300130415     C                             %dec(%subst(VABRMA : 4 : 4)
142400130415     C                              : 4 : 0)
142500130415     C                   ELSE
142600130404     C                   EVAL      wLast6 = %len(%trim(wVATNOT_E)) - 6 + 1
142700130404     C                   EVAL      VABRMN =
142800130404     C                             %dec(%subst(wVATNOT_E : wLast6 : 6)
142900130404     C                              : 6 : 0)
143000130415     C                   ENDIF
143100130404     C***                IF        %scan('/':VABRMA) > *zeros AND
143200130404     C***                          %len(%trim(VABRMA)) > 1
143300130404     C***                EVAL      VABRMN =
143400130404     C***                           %dec(%subst(VABRMA : %scan('/':VABRMA)+1:
143500130404     C***                           %len(%trim(VABRMA))-%scan('/':VABRMA)+1)
143600130404     C***                           : 15 : 0)
143700130404     C***                ENDIF
143800130404     C*** se RMA è ancora vuoto, uso il barcode
143900130325     C                   IF        VABRMA = *blank
144000130325     C                   EVAL      VABRMA = wVATNOT_E
144100130325     C                   ENDIF
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
178600130320VATNOT_E;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;X;VATNOT_B;X;VABRD2;VATNOT_I;
178700130320X;X;VABRMA;VABNCL;VABPKB;X;VABCAS;
178800120604
178900120604
179000120604
