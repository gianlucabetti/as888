000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040119     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900040128     D tisi95ds      e ds
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400010330     D rrnum           s              6  0 INZ(*zeros)
002500040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
002600040510     D parccm          s              8    INZ(*blanks)
002700040119     D parmbr          s             10    INZ(*blanks)
002800040119     D paropz          s              1    INZ(*blanks)
002900040119     D chkcall         s              1    INZ(*blanks)
003000080125     D wFlgCAS         s              1    INZ(*blanks)
003100030715     D*------------------
003200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003300030715     D*------------------
003400030715     D SkSplitFLD      S             10    DIM(100)
003500030715     D SkSplitCSV      S            256    DIM(100)
003600030715     D CharCSV         S              1
003700030715     D CharTXT         S              1
003800030715     D CharNUM         S              1
003900030715     D posDa           S              3  0 INZ(*zeros)
004000030715     D posA            S              3  0 INZ(*zeros)
004100030715     D i               s              3  0 INZ(1)
004200030715     D wGiro           s              1  0 INZ(*zeros)
004300080125     D wVATNOT_A       s                   LIKE(VATNOT)
004400080125     D wVATNOT_B       s                   LIKE(VATNOT)
004500080125     D wVATNOT_E       s                   LIKE(VATNOT)
004600030822     D*------------------
004700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004800030822     D*------------------
004900030822     D posDaDft        S              3  0 INZ(*zeros)
005000030822     D posADft         S              3  0 INZ(*zeros)
005100030822     D j               s              3  0 INZ(1)
005200030822     D wGiroDft        s              1  0 INZ(*zeros)
005300010330     D*------------------
005400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010330     D*------------------
005600010330     D WLBDA8          DS                  INZ
005700010330     D  G08DAT                 1      8  0
005800010330     D  G08INV                 9     16  0
005900010330     D  G08ERR                17     17
006000010330     D  G08TGI                18     22  0
006100080625     D*------------------
006200080625     D* DS CALCOLO CHECK-DIGITS MOD103 (TRUL28R4)
006300080625     D*------------------
006400090916     D TRUL28DSX     E DS
006500031201     D*------------------
006600031201     D* Costanti
006700031201     D*------------------
006800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007000080617     D*------------------
007100080617     D* VARIABILI X "XCHKCHAR"
007200080617     D*------------------
007300080617     D TxtInOut        S           2048
007400080617     D ElencoChar      S            256
007500080617     D TipoElenco      S              1
007600080617     D CharSost        S              1
007700080617     D UpperCase       S              1
007800080617     D ChkNull         S              1
007900080617     D CharNull        S              1
008000080617     D EsitoBon        S              1
008100080617     D*------------------
008200080617     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
008300080617     D*------------------
008400080617     D PiStr           S            100A   INZ
008500080617     D PiDecChr        S              1A   INZ
008600080617     D PiVal           S             63S30 INZ
008700080617     D PiInt           S               N   INZ
008800080617     D PiNum           S               N   INZ
008900080617
009000080617     D*------------------
009100080617     D* LINKING A DEFINIZIONI ESTERNE
009200080617     D*------------------
009300080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009400010330
009500010330
009600010330
009700000913     C                   reset                   rrnum
009800990921     C                   reset                   esito
009900990921     C                   reset                   wrkesito
010000010601     C*
010100010601     C                   exsr      opeini
010200010605     C                   exsr      rwvab
010300010601     C*
010400010601     C                   seton                                        lr
010500010601
010600010601
010700010601
010800010601     C*--------------------------------------------------------
010900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011000010601     C*--------------------------------------------------------
011100010601     C     PREELA        BEGSR
011200010601     C*
011300010601     C* SE OCCORRE SPEDIRE IN FILIALE
011400010601     C                   if        invfil <> *zeros and
011500010601     C                             flgGiro = '0'
011600010601     C*
011700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011800010601     C                   eval      flgGiro = '1'
011900010601     C*
012000010601     C                   endif
012100010601     C*
012200010601     C                   ENDSR
012300010601     C***
012400010601
012500010601
012600010601
012700010601
012800010601     C*--------------------------------------------------------
012900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013000010601     C*--------------------------------------------------------
013100010601     C     ENDELA        BEGSR
013200040119     C*
013300040119     C* Effettuo la chiamata al CLLE preposto
013400040119     C                   call(e)   'TITVVTC'
013500040510     C                   parm                    parccm
013600040119     C                   parm                    parmbr
013700040119     C                   parm      '2'           paropz
013800050201     C*
013900050201     C* Effettuo lancio TISI95 solo x chiusura
014000050201     C                   CLEAR                   TISI95DS
014100050201     C                   EVAL      I95TLA = 'C'
014200050201     C                   CALL      'TISI95R'
014300050201     C                   PARM                    TISI95DS
014400000616     C*
014500010601     C                   ENDSR
014600010601     C***
014700000613
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015200010330     C*--------------------------------------------------------
015300010605     C     RWVAB         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800021025     C                   if        not %open(fivabwwr)
015900021025     C                   open      fivabwwr
016000010330     C                   endif
016100040119     C*
016200040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300040119     C                   exsr      prevat
016400040119     C*
016500040119     C                   if        chkcall = '0'
016600040119     C*
016700040119     C                   if        not %open(fivatwwr)
016800040119     C                   open      fivatwwr
016900040119     C                   endif
017000010330     C*
017100040119     C                   clear                   §CTROKVB          7 0
017200040119     C                   clear                   §CTROKVT          7 0
017300040119     C                   clear                   §CTRMO            7 0
017400040119     C                   clear                   §CTRNO            7 0
017500070823     C*
017600070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017700070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017800080108     C***                exsr      inzvar
017900080108     C***                exsr      impvab
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
021800010330     C                   exsr      inzvar
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
023800030822     C                   if        wGiro = 2
023900040510     C* Scrivo sempre il VAT
024000040510     C  N31              exsr      wrivat
024100040510     C* Scrivo il VAB solo a rottura d spedizione
024200040119     C                   if        depspe =  *zeros OR
024300040119     C                             depspe <> vabNSP
024400040119     C  N31              exsr      wrivab
024500040119     C  N31              eval      depspe = vabNSP
024600040119     C                   endif
024700040119     C                   endif
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
031800040119
031900040119
032000040119
032100040119     C*----------------------------------------------------*
032200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
032300040119     C*----------------------------------------------------*
032400040510     C     WRIVAB        BEGSR
032500040119     C*
032600060331     C* Gestisco l'eventuale rottura x numero spedizione
032700070823     C                   if        VABNSP <> depspe
032800070301     C                   if        wVATNOT_E <> *blanks
032900080125     C                   if        VABCTM = *blanks
033000080125     C                   eval      VABCTM = '7Q'
033100080125     C                   endif
033200070301     C                   endif
033300040119     C                   write     fivab000                                     => scarico il VAB
033400060331     C                   endif
033500040119     C*
033600040119     C                   ENDSR
033700040119
033800040119
033900040119
034000040119     C*----------------------------------------------------*
034100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
034200040119     C*----------------------------------------------------*
034300040510     C     WRIVAT        BEGSR
034400060512     C*
034500060512     C* Inizializzo i campi chiave
034600060512     C                   eval      VATFGS = VABFGS
034700060512     C                   eval      VATAAS = VABAAS
034800060512     C                   eval      VATCCM = VABCCM
034900070823     C                   eval      VATNRS = VABNRS
035000060512     C                   eval      VATNSP = VABNSP
035100060512     C                   eval      VATLNP = VABLNP
035200060331     C*
035300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
035400060331     C                   if        wVATNOT_A <> *blanks
035500060331     C                   eval      VATNOT = wVATNOT_A
035600060331     C                   eval      VATTRC = 'A'
035700060331     C                   write     FIVAT000
035800060331     C                   add       1             §CTROKVT
035900060331     C                   endif
036000060331     C*
036100060331     C                   if        wVATNOT_B <> *blanks
036200060331     C                   eval      VATNOT = wVATNOT_B
036300060331     C                   eval      VATTRC = 'B'
036400060331     C                   write     FIVAT000
036500060331     C                   add       1             §CTROKVT
036600060331     C                   endif
036700060331     C*
036800080625     C                   if        wVATNOT_E <> *blanks
036900060331     C                   eval      VATNOT = wVATNOT_E
037000060331     C                   eval      VATTRC = 'E'
037100080625     C*
037200080625     C                   z-add     *zeros        wBarcodeDA       11 0
037300080625     C                   z-add     *zeros        wBarcodeA        11 0
037400090706     C                   if        %len(%trim(wVATNOT_E)) < 13
037500090706     C                   seton                                        32
037600090706     C                   eval      vinmsg = %trimr(vinmsg)
037700090706     C                             + ' ' + 'VATNOT_E'
037800090706     C                   else
037900080625     C                   eval      PiStr=%subst(%trim(wVATNOT_E):3:11)
038000080625     C                   exsr      CHKNUM
038100080625     C                   if        PiInt=*on
038200080625     C*
038300080625     C                   z-add     PiVal         wBarcodeDA
038400080625     C                   eval      wBarcodeA = wBarcodeDA + VABNCL - 1
038500080625     C*
038600080625     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
038700080625     C     wBarcodeDA    DO        wBarcodeA     wBarcodeNum      11 0
038800080625     C*
038900080625     C* Compongo la parte variabile
039000080625     C                   MOVE(P)   wBarcodeNum   wBarcodeAlfa     11
039100080625     C* Eseguo calcolo check-digit
039200090916     C                   CLEAR                   TRUL28DSX
039300090916     C                   EVAL      I284STD = 'E10'
039400080625     C                   EVAL      I284COD = wBarcodeAlfa
039500080625     C                   CALL      'TRUL28R4'
039600090916     C                   PARM                    TRUL28DSX
039700080625     C* Se tutto ok scarico il buffer del FNVAT
039800080625     C                   IF        O284ERR <> '1'
039900080625     C                   EVAL      VATNOT = 'AA'+%trim(O284COD)
040000080625     C                   WRITE     FIVAT000
040100080625     C                   ADD       1             §CTROKVT
040200080625     C                   ELSE
040300080625     C                   SETON                                        32
040400080625     C                   EVAL      vinmsg = %trimr(vinmsg)
040500080625     C                             + ' ' + 'VATNOT'
040600080625     C                   ENDIF
040700080625     C                   ENDDO
040800080625     C*
040900090706     C                   else
041000080625     C                   seton                                        32
041100080625     C                   eval      vinmsg = %trimr(vinmsg)
041200080625     C                             + ' ' + 'VATNOT_E'
041300080625     C                   endif
041400060331     C                   endif
041500080625     C                   endif
041600040119     C*
041700040119     C                   ENDSR
041800040119
041900040119
042000040119
042100040119     C*----------------------------------------------------*
042200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042300040119     C*----------------------------------------------------*
042400040119     C     PREVAT        BEGSR
042500040119     C*
042600040119     C* Compongo il nome del membro da dare al FIVATWWR
042700040119     C                   eval      parmbr = vlrhdl
042800040119     C                   movel     'M'           parmbr
042900040510     C                   eval      parccm = vlrksc
043000040119     C                   eval      paropz = '1'
043100040119     C* Effettuo la chiamata al CLLE preposto
043200040119     C                   call(e)   'TITVVTC'
043300040119     C                   parm                    parccm
043400040119     C                   parm                    parmbr
043500040119     C                   parm                    paropz
043600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043700040119     C                   if        %error
043800040119     C                   movel     '1'           chkcall
043900040119     C                   else
044000040119     C                   movel     '0'           chkcall
044100040119     C                   endif
044200040119     C*
044300040119     C                   ENDSR
044400010601
044500010601
044600010601
044700010330     C*----------------------------------------------------*
044800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044900010330     C*----------------------------------------------------*
045000040119     C     INZVAR        BEGSR
045100010330     C*
045200030822     C* Inizializzo variabili di wrk
045300020204     C                   Z-ADD     *zeros        Num5_0            5 0
045400060331     C                   MOVEL     *blanks       wVATNOT_A
045500060331     C                   MOVEL     *blanks       wVATNOT_B
045600060331     C                   MOVEL     *blanks       wVATNOT_E
045700080617     C                   EVAL      wFlgCAS = *blanks
045800030822     C*
045900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046000030822     C                   CLEAR                   FIVAB000
046100040119     C                   CLEAR                   FIVAT000
046200030822     C                   CLEAR                   SkSplitCSV
046300030822     C                   EVAL      i = 1
046400030822     C                   EVAL      posDa = *zeros
046500030822     C                   EVAL      posA  = *zeros
046600030822     C*
046700030822     C* Reimposto i valori di default
046800030822     C                   EXSR      DEFCAM
046900030822     C*
047000010330     C                   ENDSR
047100010330     C*----------------------------------------------------*
047200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047300010330     C*----------------------------------------------------*
047400010330     C     DEFCAM        BEGSR
047500030822     C*
047600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047700030715     C* e delimitatore testo.
047800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
047900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048100030822     C*
048200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
048300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
048400030715     C                   SETON                                        50
048500030715     C                   ELSE
048600030715     C                   SETOFF                                       50
048700030715     C                   ENDIF
048800030822     C*
048900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
049000030822     C                   EVAL      posDaDft = 1
049100030822     C                   EVAL      posADft  = 0
049200030822     C                   EVAL      wGiroDft = 0
049300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
049400030822     C                             posDaDft > 0
049500030822     C*
049600030822     C* Gestisco il 1° giro
049700030822     C                   IF        wGiroDft = 0
049800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
049900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
050000030822     C* Incremento il contatore dei "giri"
050100030822     C                   EVAL      wGiroDft = 1
050200030822     C                   ELSE
050300030822     C                   EVAL      posDaDft = posADft
050400030822     C                   ENDIF
050500030822     C* Eseguo lo scan x trovare la fine del campo corrente
050600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
050700030822     C*
050800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
050900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
051000030822     C                   IF        posDaDft > 0 AND
051100030822     C                             posADft  > 0
051200030822     C* NCL
051300030822     C                   IF        %subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):1:3)
051600030822     C                             = 'NCL'
051700030822     C                   EVAL      PiStr=%trim(%subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000030822     C                   EXSR      CHKNUM
052100030822     C                   IF        PiInt=*on
052200030822     C                   Z-ADD     PiVal         VABNCL
052300030822     C                   ENDIF
052400030822     C                   ENDIF
052500030822     C* CCM
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'CCM'
053000030822     C                   EVAL      PiStr=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   EXSR      CHKNUM
053400030822     C                   IF        PiInt=*on
053500030822     C                   Z-ADD     PiVal         VABCCM
053600030822     C                   ENDIF
053700030822     C                   ENDIF
053800030822     C* LNP
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'LNP'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiInt=*on
054800030822     C                   Z-ADD     PiVal         VABLNP
054900030822     C                   ENDIF
055000030822     C                   ENDIF
055100030822     C* NRS
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'NRS'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiInt=*on
056100030822     C                   Z-ADD     PiVal         VABNRS
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* CTR
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'CTR'
056900030822     C                   EVAL      PiStr=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   EXSR      CHKNUM
057300030822     C                   IF        PiInt=*on
057400030822     C                   Z-ADD     PiVal         VABCTR
057500030822     C                   ENDIF
057600030822     C                   ENDIF
057700030822     C* PKB
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'PKB'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiNum=*on
058700030822     C                   Z-ADD     PiVal         VABPKB
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* VLB
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'VLB'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiNum=*on
060000030822     C                   Z-ADD     PiVal         VABVLB
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* QFT
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'QFT'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiNum=*on
061300030822     C                   Z-ADD     PiVal         VABQFT
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C* CBO
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'CBO'
062100030822     C                   EVAL      VABCBO=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   ENDIF
062500030822     C* TSP
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'TSP'
063000030822     C                   EVAL      VABTSP=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   ENDIF
063400030822     C* VAS
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'VAS'
063900030822     C                   EVAL      VABVAS=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   ENDIF
064300030822     C* VCA
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'VCA'
064800030822     C                   EVAL      VABVCA=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200070925     C* TIC
065300070925     C                   IF        %subst(
065400070925     C                             %subst(vlrppt:posDaDft+1:
065500070925     C                             posADft-posDaDft-1):1:3)
065600070925     C                             = 'TIC'
065700070925     C                   EVAL      VABTIC=%trim(%subst(
065800070925     C                             %subst(vlrppt:posDaDft+1:
065900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000070925     C                   ENDIF
066100030822     C* GCA
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'GCA'
066600030822     C                   EVAL      VABGCA=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* CTM
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'CTM'
067500030822     C                   EVAL      VABCTM=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* FFD
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'FFD'
068400030822     C                   EVAL      VABFFD=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* VAD
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'VAD'
069300030822     C                   EVAL      VABVAD=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* GMA
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'GMA'
070200030822     C                   EVAL      VABGMA=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* GGA
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'GGA'
071100030822     C                   EVAL      VABGGA=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* GVA
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'GVA'
072000030822     C                   EVAL      VABGVA=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* TC1
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'TC1'
072900030822     C                   EVAL      VABTC1=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* TC2
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'TC2'
073800030822     C                   EVAL      VABTC2=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200040714     C* VATTRC
074300040714     C                   IF        %subst(
074400040714     C                             %subst(vlrppt:posDaDft+1:
074500040714     C                             posADft-posDaDft-1):1:3)
074600040714     C                             = 'TRC'
074700040714     C                   EVAL      VATTRC=%trim(%subst(
074800040714     C                             %subst(vlrppt:posDaDft+1:
074900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000040714     C                   ENDIF
075100030822     C* ...
075200030822     C                   ENDIF
075300030822     C                   ENDDO
075400020204     C*
075500010330     C                   ENDSR
075600010607     C*----------------------------------------------------*
075700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075800010607     C*----------------------------------------------------*
075900010607     C     IMPVAB        BEGSR
076000010607     C*
076100010607     C                   SETOFF                                       3132
076200030515     C*
076300010607     C                   MOVEL     datcor        VABAAS
076400010607     C                   MOVE      datcor        VABMGS
076500010607     C*
076600030715     C**********
076700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076800030715     C**********
076900030715     C                   IF        wGiro = *zeros
077000040113     C*
077100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
077200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
077300040510     C                             %trim(intcol(3))<>*blanks
077400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077500040113     C                                      %trim(intcol(3))
077600070823     C     ';':CharCSV   XLATE     vindta        vindta
077700040510     C                   ENDIF
077800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077900031201     C     minu:maiu     XLATE     vindta        vindta
078000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078100030715     C                   DOW       posDa <= %len(%trim(vindta))
078200030715     C*
078300030715     C* Gestisco il 1° campo
078400030715     C                   IF        i = 1
078500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078600030715     C                   EVAL      posDa = 1
078700030715     C* Eseguo lo scan x trovare la fine del primo campo
078800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078900030715     C* A questo punto "estraggo" il campo corrente
079000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079100030715     C                                                    (posA-posDa))
079200030715     C* X i campi successivi al 1°
079300030715     C                   ELSE
079400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079500030715     C                   EVAL      posDa = posA + 1
079600030715     C* Eseguo lo scan x trovare la fine del campo corrente
079700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079800030715     C* Gestisco l'ultimo campo
079900030715     C                   IF        posA = *zeros
080000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080100030715     C* A questo punto "estraggo" il campo corrente
080200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080300030715     C                                                    (posA-posDa))
080400030715     C                   LEAVE
080500030715     C                   ELSE
080600030715     C* A questo punto "estraggo" il campo corrente
080700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080800030715     C                                                    (posA-posDa))
080900030715     C                   ENDIF
081000030715     C                   ENDIF
081100030715     C* Incremento il contatore d campo
081200030715     C                   EVAL      i = i +1
081300030715     C                   ENDDO
081400030715     C*
081500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081600030715     C* delimitazione testo
081700030715     C                   EVAL      i = 1
081800030715     C                   DOW       i < %elem(SkSplitFLD)
081900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082100030715     C                   EVAL      i = i + 1
082200030715     C                   ENDDO
082300030715     C                   EVAL      wGiro = 1
082400030715     C                   ELSE
082500030715     C                   EVAL      wGiro = 2
082600031204     C**********
082700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082800031204     C* d separatore campo
082900031204     C**********
083000031204     C                   EVAL      vindta = %trim(vindta)
083100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083200031204     C                   EVAL      vindta = %subst(vindta:2)
083300031204     C                   ENDDO
083400031204     C*
083500031204     C                   Z-ADD     *zeros        lunghInput        4 0
083600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084000031204     C                   ENDDO
084100030715     C**********
084200030715     C* Effettuo lo split del campo dati d input
084300030715     C**********
084400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084500030715     C                   DOW       posDa <= %len(%trim(vindta))
084600030715     C*
084700030715     C* Gestisco il 1° campo
084800030715     C                   IF        i = 1
084900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085000030715     C                   EVAL      posDa = 1
085100030715     C* Eseguo lo scan x trovare la fine del primo campo
085200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085300030715     C* A questo punto "estraggo" il campo corrente
085400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085500030715     C                                                    (posA-posDa))
085600030715     C* X i campi successivi al 1°
085700030715     C                   ELSE
085800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085900030715     C                   EVAL      posDa = posA + 1
086000031204     C* Verifico che nn vi sia il campo nn valorizzato
086100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086200031204     C* Se campo nn valorizzato skippo al prossimo
086300031204     C                   EVAL      SkSplitCSV(i) = *blanks
086400031204     C                   EVAL      posA  = posA + 1
086500031204     C                   ELSE
086600030715     C* Eseguo lo scan x trovare la fine del campo corrente
086700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086800030715     C* Gestisco l'ultimo campo
086900030715     C                   IF        posA = *zeros
087000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087100030715     C* A questo punto "estraggo" il campo corrente
087200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087300030715     C                                                    (posA-posDa))
087400030715     C                   LEAVE
087500030715     C                   ELSE
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C                   ENDIF
088000031204     C                   ENDIF
088100030715     C                   ENDIF
088200030715     C* Incremento il contatore d campo
088300030715     C                   EVAL      i = i +1
088400030715     C                   ENDDO
088500030715     C*
088600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088700030715     C* delimitazione testo
088800030715     C                   EVAL      i = 1
088900030715     C                   DOW       i < %elem(SkSplitCSV)
089000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089200030715     C                   EVAL      i = i + 1
089300030715     C                   ENDDO
089400030715     C*
089500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089600030715     C                   EVAL      i = 1
089700030715     C                   DOW       i < %elem(SkSplitFLD)
089800030715     C*
089900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090100030715     C                             SkSplitCSV(i) <> *blanks
090200030721     C*** ATB
090300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090400030721     C                   EVAL      VABATB = SkSplitCSV(i)
090500030721     C                   ENDIF
090600030715     C*** RSD
090700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091100030715     C* ==
091200030715     C                   ENDIF
091300030721     C*** RD2
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030715     C*** IND
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091900030715     C                   EVAL      VABIND = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030715     C*** LOD
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030715     C*** PRD
092600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092800030715     C                   ENDIF
092900030721     C*** NZD
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030715     C*** RMA
093400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093600030715     C                   ENDIF
093700030715     C*** NOT
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030715     C*** NT2
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** VCA
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** VAS
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** FFD
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030721     C*** GC1
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** GC2
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** TSP
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096800030721     C                   ENDIF
096900040510     C*** CBO
097000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
097100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
097200040510     C                   ENDIF
097300030721     C*** NAS
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** TIC
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** GCA
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** XCO
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** CTM
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** TCR
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** CTS
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** FTM
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** VAD
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** GMA
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** GGA
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** GVA
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** TC1
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** TC2
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** SCL
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** RMO
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** NMO
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*
104200030721     C* Reperisco quindi i campi numerici...
104300040510     C*** NSP
104400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104500040510     C                   EVAL      PiStr=SkSplitCSV(i)
104600040510     C                   EXSR      CHKNUM
104700040510     C                   IF        PiInt=*on
104800040510     C                   Z-ADD     PiVal         VABNSP
104900040510     C                   ELSE
105000040510     C                   SETON                                        31
105100040510     C                   EVAL      VABNSP = *zeros
105200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
105300040510     C                             + ' ' + 'VABNSP'
105400040510     C                   ENDIF
105500040510     C                   ENDIF
105600071106     C*** RMN
105700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105800071106     C                   EVAL      PiStr=SkSplitCSV(i)
105900071106     C                   EXSR      CHKNUM
106000071106     C                   IF        PiInt=*on
106100071106     C                   Z-ADD     PiVal         VABRMN
106200071106     C                   ELSE
106300071106     C                   SETON                                        32
106400071106     C                   EVAL      VABRMN = *zeros
106500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
106600071106     C                             + ' ' + 'VABRMN'
106700071106     C                   ENDIF
106800071106     C                   ENDIF
106900030721     C*** CAD
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
107100030721     C                   EVAL      PiStr=SkSplitCSV(i)
107200030721     C                   EXSR      CHKNUM
107300030721     C                   IF        PiInt=*on
107400030721     C                   Z-ADD     PiVal         Num5_0
107500030721     C                   MOVEL(p)  Num5_0        VABCAD
107600030721     C                   ELSE
107700030721     C                   SETON                                        32
107800030721     C                   EVAL      VABCAD = *zeros
107900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108000030721     C                             + ' ' + 'VABCAD'
108100030721     C                   ENDIF
108200030721     C                   ENDIF
108300030721     C*** NCL
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108500030721     C                   EVAL      PiStr=SkSplitCSV(i)
108600030721     C                   EXSR      CHKNUM
108700030721     C                   IF        PiInt=*on
108800030721     C                   Z-ADD     PiVal         VABNCL
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      VABNCL = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300040122     C                             + ' ' + 'VABNCL'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** PKB
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiNum=*on
110100030721     C                   Z-ADD     PiVal         VABPKB
110200030721     C                   ELSE
110300030721     C                   SETON                                        32
110400030721     C                   EVAL      VABPKB = *zeros
110500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110600030721     C                             + ' ' + 'VABPKB'
110700030721     C                   ENDIF
110800030721     C                   ENDIF
110900030721     C*** CAS
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
111100080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
111200080617     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
111300080617     C                             %trim(SkSplitCSV(i)) <> '0'
111400080125     C                   EVAL      wFlgCAS = '1'
111500080125     C                   ENDIF
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiNum=*on
111900030721     C                   Z-ADD     PiVal         VABCAS
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      VABCAS = *zeros
112300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112400030721     C                             + ' ' + 'VABCAS'
112500030721     C                   ENDIF
112600030721     C                   ENDIF
112700030721     C*** CCM
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112900030721     C                   EVAL      PiStr=SkSplitCSV(i)
113000030721     C                   EXSR      CHKNUM
113100030721     C                   IF        PiInt=*on      AND
113200030721     C                             PiVal<=9999999 AND
113300030721     C                             PiVal>*zeros
113400030721     C                   Z-ADD     PiVal         VABCCM
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      VABCCM = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900040510     C                             + ' ' + 'VABCCM'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** LNP
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on    AND
114700030721     C                             PiVal<=999   AND
114800030721     C                             PiVal>*zeros
114900030721     C                   Z-ADD     PiVal         VABLNP
115000030721     C                   ELSE
115100040715     C                   SETON                                        32
115200030721     C                   EVAL      VABLNP = *zeros
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400040510     C                             + ' ' + 'VABLNP'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** NRS
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiInt=*on AND
116200030721     C                             PiVal<=99
116300030721     C                   Z-ADD     PiVal         VABNRS
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABNRS'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** LNA
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on    AND
117500030721     C                             PiVal<=999   AND
117600030721     C                             PiVal>*zeros
117700030721     C                   Z-ADD     PiVal         VABLNA
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      VABLNA = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200030721     C                             + ' ' + 'VABLNA'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** CTR
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiInt=*on  AND
119000030721     C                             PiVal<=999
119100030721     C                   Z-ADD     PiVal         VABCTR
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABCTR'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** IAS
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiNum=*on
120300030721     C                   Z-ADD     PiVal         VABIAS
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABIAS'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** VLB
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiNum=*on
121500030721     C                   Z-ADD     PiVal         VABVLB
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABVLB'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** QFT
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiNum=*on
122700030721     C                   Z-ADD     PiVal         VABQFT
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABQFT'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** NCD
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on      AND
123900030721     C                             PiVal<=9999999
124000030721     C                   Z-ADD     PiVal         VABNCD
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABNCD'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** NCA
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on      AND
125200030721     C                             PiVal<=9999999
125300030721     C                   Z-ADD     PiVal         VABNCA
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABNCA'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** ZNC
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiInt=*on AND
126500030721     C                             PiVal<=99
126600030721     C                   Z-ADD     PiVal         VABZNC
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABZNC'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** DCR
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on       AND
127800030721     C                             PiVal<=99999999
127900030721     C                   Z-ADD     PiVal         VABDCR
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABDCR'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** HCR
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on   AND
129100030721     C                             PiVal<=9999
129200030721     C                   Z-ADD     PiVal         VABHCR
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABHCR'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** VMD
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiNum=*on
130400030721     C                   Z-ADD     PiVal         VABVMD
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABVMD'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** ANT
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiInt=*on        AND
131600030721     C                             PiVal<=999999999
131700030721     C                   Z-ADD     PiVal         VABANT
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABANT'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** CMO
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on
132900030721     C                   Z-ADD     PiVal         Num5_0
133000030721     C                   MOVEL(p)  Num5_0        VABCMO
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      VABCMO = *zeros
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABCMO'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800040510     C*
133900040510     C* Reperisco i campi del FIVAT
134000040510     C*** VATNOT
134100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
134200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
134300040510     C                   ENDIF
134400040510     C*** VATTRC
134500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
134600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
134700040510     C                   ENDIF
134800060331     C*** VATNOT_A
134900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
135000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
135100060331     C                   ENDIF
135200060331     C*** VATNOT_B
135300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
135400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
135500060331     C                   ENDIF
135600060331     C*** VATNOT_E
135700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
135800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
135900060331     C                   ENDIF
136000070823     C*
136100071029     C***  ===>  Gestione campi "particolari" dentro ciclo
136200100607     C*
136300100607     C*** RSD/RD2
136400100607     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
136500100607     C                   MOVEL     *blanks       wDEST            70
136600100607     C                   EVAL      wDEST  = SkSplitCSV(i)
136700100607     C                   EVAL      VABRSD = %subst(wDEST:1:35)
136800100607     C                   EVAL      VABRD2 = %subst(wDEST:36:35)
136900100607     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
137000100607     C     '@':'A'       XLATE     VABRSD        VABRSD
137100100607     C                   ENDIF
137200071204     C*
137300100607     C*** NSP/RMN
137400080617     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
137500110314     C***                EVAL      TxtInOut = %trim(SkSplitCSV(i))
137600110314     C***                EXSR      exeBon
137700110314     C***                EVAL      PiStr=TxtInOut
137800110314     C                   EVAL      PiStr = %trim(SkSplitCSV(i))
137900071204     C                   EXSR      CHKNUM
138000071204     C                   IF        PiInt=*on
138100071204     C                   Z-ADD     PiVal         VABNSP
138200071204     C                   Z-ADD     PiVal         VATNSP
138300071204     C                   Z-ADD     PiVal         VABRMN
138400071204     C                   ELSE
138500071204     C                   SETON                                        32
138600071204     C                   EVAL      VABNSP = *zeros
138700071204     C                   EVAL      VATNSP = *zeros
138800071204     C                   EVAL      VABRMN = *zeros
138900071204     C                   EVAL      vinmsg = %trimr(vinmsg)
139000071204     C                             + ' ' + 'VABNSP VATNSP VABRMN'
139100071204     C                   ENDIF
139200071204     C                   ENDIF
139300071204     C*
139400080617     C*** CBO
139500080617     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
139600080617     C                   IF        %trim(SkSplitCSV(i)) = 'FRANCO'
139700080617     C                   EVAL      VABCBO = '1'
139800080617     C                   ENDIF
139900080617     C                   IF        %trim(SkSplitCSV(i)) = 'ASSEGNATO'
140000080617     C                   EVAL      VABCBO = '2'
140100080617     C                   ENDIF
140200080617     C                   ENDIF
140300080617     C*
140400080617     C*** FFD
140500080617     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABFFD'
140600080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
140700080617     C                   EVAL      VABFFD = 'S'
140800080617     C                   ENDIF
140900080617     C                   ENDIF
141000070925     C*
141100071029     C***  <===  -----------------------------------------
141200040510     C*
141300030715     C                   ENDIF
141400030715     C                   EVAL      i = i + 1
141500030715     C                   ENDDO
141600070925     C*
141700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
141800080125     C*
141900070925     C*
142000071029     C***  <===  -----------------------------------------
142100070925     C*
142200070925     C* Considerazioni finali su CBO/CAS
142300080125     C                   IF        wFlgCAS = '1'
142400070925     C                   IF        VABCBO = '1'
142500070925     C                   EVAL      VABCBO = '4'
142600070925     C                   ENDIF
142700070925     C                   IF        VABCBO = '2'
142800070925     C                   EVAL      VABCBO = '6'
142900070925     C                   ENDIF
143000070925     C                   ENDIF
143100070925     C***  <===  ----------------------------
143200040510     C*
143300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
143400040510     C                   IF        VABNSP = *zeros
143500040510     C                   SETON                                        31
143600040510     C                   ENDIF
143700040715     C*
143800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
143900040715     C                   IF        VABLNP = *zeros
144000040715     C                   SETON                                        31
144100040715     C                   ENDIF
144200040510     C*
144300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
144400040510     C                   IF        VABRMN = *zeros
144500040510     C                   Z-ADD     VABNSP        VABRMN
144600040510     C                   ENDIF
144700040510     C*
144800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
144900040510     C                   IF        VABRMA = *blanks
145000040714     C                   MOVEL     VABRMN        VABRMA
145100040510     C                   ENDIF
145200040128     C*
145300040510     C* Se provincia nn valorizzata la reperisco
145400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
145500040510     C                   IF        VABPRD = *blanks
145600040128     C                   CLEAR                   TISI95DS
145700040128     C                   EVAL      I95TCN = '3'
145800040128     C                   Z-ADD     datcor        I95DAT
145900040128     C                   EVAL      I95NAR = VABNZD
146000040128     C                   EVAL      I95CAP = VABCAD
146100040128     C                   EVAL      I95LOC = VABLOD
146200040128     C                   CALL      'TISI95R'
146300040128     C                   PARM                    TISI95DS
146400040128     C                   EVAL      VABPRD = O95PRV
146500040510     C                   ENDIF
146600020204     C*
146700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
146800020204     C                   EXSR      CHKIMPDIV
146900030715     C                   ENDIF
147000020204     C*
147100010607     C                   ENDSR
147200010607     C*----------------------------------------------------*
147300010601
147400020204
147500020204     C*----------------------------------------------------*
147600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
147700020204     C*----------------------------------------------------*
147800020204     C     CHKIMPDIV     BEGSR
147900020204     C*
148000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
148100020204     C                   Z-ADD     *zeros        wrkDec            9 9
148200020204     C*
148300020204     C* Come prima cosa effettuo considerazioni sulla divisa
148400020204     C                   IF        vabIAS > *zeros
148500020204     C                   IF        vabVAS <> 'EUR'
148600020204     C                   EVAL      vabVAS =  'ITL'
148700020204     C                   ENDIF
148800020204     C                   ENDIF
148900020204     C*
149000020204     C                   IF        vabCAS > *zeros
149100020204     C                   IF        vabVCA <> 'EUR'
149200020204     C                   EVAL      vabVCA =  'ITL'
149300020204     C                   ENDIF
149400020204     C                   ENDIF
149500020204     C*
149600020204     C                   IF        vabVMD > *zeros
149700020204     C                   IF        vabVAD <> 'EUR'
149800020204     C                   EVAL      vabVAD =  'ITL'
149900020204     C                   ENDIF
150000020204     C                   ENDIF
150100020204     C*
150200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
150300020204     C                   Z-ADD     vabIAS        wrkDec
150400020204     C                   IF        wrkDec > *zeros
150500020204     C                   IF        vabVAS = 'ITL'
150600020204     C                   EVAL      vabIAS = *zeros
150700020204     C                   ENDIF
150800020204     C                   ENDIF
150900020204     C*
151000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
151100020204     C                   Z-ADD     vabCAS        wrkDec
151200020204     C                   IF        wrkDec > *zeros
151300020204     C                   IF        vabVCA = 'ITL'
151400020204     C                   EVAL      vabCAS = *zeros
151500020204     C                   ENDIF
151600020204     C                   ENDIF
151700020204     C*
151800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
151900020204     C                   Z-ADD     vabVMD        wrkDec
152000020204     C                   IF        wrkDec > *zeros
152100020204     C                   IF        vabVAD = 'ITL'
152200020204     C                   EVAL      vabVMD = *zeros
152300020204     C                   ENDIF
152400020204     C                   ENDIF
152500020204     C*
152600020204     C                   ENDSR
152700020204     C***
152800080617
152900080617
153000080617
153100080617     C*----------------------------------------------------*
153200080617     C*  CONTROLLO NUMERICITA' CAMPI
153300080617     C*----------------------------------------------------*
153400080617     C     CHKNUM        BEGSR
153500080617     C*
153600080617     C                   IF        PiDecChr = *blanks
153700080617     C                   EVAL      PiDecChr = CharNUM
153800080617     C                   ENDIF
153900080617     C*
154000080617     C                   callp     UBISNUM_Check(PiStr
154100080617     C                                          :PiDecChr
154200080617     C                                          :PiVal
154300080617     C                                          :PiNum
154400080617     C                                          :PiInt)
154500080617     C*
154600080617     C                   ENDSR
154700080617     C***
154800010330
154900010601
155000010601
155100010601
155200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
155300010601     C     repfil        BEGSR
155400010601     C*
155500010601     C                   if        invfil = *zeros and
155600010601     C                             depfil > *zeros and
155700010629     C                             (vinflg = *blanks or
155800010629     C                              vinflg = *zeros)
155900010601     C
156000010601     C                   eval      invfil = depfil
156100010601     C                   endif
156200010601     C*
156300010601     C                   if        depfil <> invfil and
156400010601     C                             invfil > *zeros
156500010601     C                   eval      flgMulti = '1'
156600010601     C                   if        vinflg = *blanks
156700010601     C                   add       1             cntNonEl
156800010601     C                   endif
156900010601     C                   endif
157000010601     C*
157100010601     C                   if        vinflg = '2'
157200010601     C                   eval      flgStato = '2'
157300010601     C                   endif
157400010601     C*
157500010601     C                   ENDSR
157600010601     C***
157700010601
157800010601
157900010601
158000010330
158100040119      /TITLE Invio dei dati al punto operativo.
158200040119     C     invio         BEGSR
158300040119     C*
158400040119     C* 1° invio FIVAT
158500040119     C                   reset                   dscmz
158600040119     C                   move      vlrpoi        cmzdst
158700040119     C                   eval      cmzfld = 'FIVATWWR'
158800040119     C                   eval      cmzmbd = vlrhdl
158900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159000040119     C***                if        prmfir = *blanks
159100040119     C                   eval      cmzfla = 'FIVAT00F'
159200040119     C                   eval      cmzmba = 'FIVAT00F'
159300040119     C***                else
159400040119     C***                eval      cmzfla = prmfir
159500040119     C***                eval      cmzmba = prmfir
159600040119     C***                endif
159700040119     C                   eval      cmznrr = *zeros
159800040119     C                   move      §ctrokvt      cmznrr
159900040119     C                   eval      cmzlba = vlrfl1
160000040119     C                   call(e)   'TIS711C'
160100040119     C                   parm                    dscmz
160200040119     C                   parm      *blanks       esito
160300040119     C                   if        %error
160400040119     C                             or cmzerr = '1'
160500040119     C                             or esito  = '1'
160600040119     C                   eval      wrkesito = '3'
160700040119     C                   else
160800040119     C*
160900040119     C* 2° invio FIVAB
161000040119     C                   reset                   dscmz
161100040119     C                   move      vlrpoi        cmzdst
161200040119     C                   eval      cmzfld = vlrfou
161300040119     C                   eval      cmzmbd = vlrhdl
161400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161500040119     C***                if        prmfir = *blanks
161600040119     C                   eval      cmzfla = 'FIVAB00F'
161700040119     C                   eval      cmzmba = 'FIVAB00F'
161800040119     C***                else
161900040119     C***                eval      cmzfla = prmfir
162000040119     C***                eval      cmzmba = prmfir
162100040119     C***                endif
162200040119     C                   eval      cmznrr = *zeros
162300040119     C                   move      §ctrokvb      cmznrr
162400040119     C                   eval      cmzlba = vlrfl1
162500040119     C                   call(e)   'TIS711C'
162600040119     C                   parm                    dscmz
162700040119     C                   parm      *blanks       esito
162800040119     C                   if        %error
162900040119     C                             or cmzerr = '1'
163000040119     C                             or esito  = '1'
163100040119     C                   eval      wrkesito = '3'
163200040119     C                   endif
163300040119     C                   endif
163400040119     C*
163500040119     C                   ENDSR
163600040119     C***
163700010601
163800010601
163900010601
164000010601
164100010601      /TITLE Invio dei dati al punto operativo.
164200010601     C     opeini        BEGSR
164300010601     C*
164400010601     C* Inizializzo flag e contatori operativi
164500010601     C                   movel     '0'           flgGiro           1
164600010601     C                   movel     '0'           flgMulti          1
164700010601     C                   movel     '1'           flgStato          1
164800010615     C                   movel     '0'           flgOk             1
164900010601     C                   z-add     *zeros        cntNonEl         10 0
165000010601     C                   z-add     *zeros        depfil            3 0
165100010601     C                   z-add     *zeros        invfil            3 0
165200010601     C*
165300010601     C                   ENDSR
165400010601     C***
165500070326
165600070326
165700070326
165800070326
165900070326     C     *pssr         BEGSR
166000070329     C*
166100070329     C                   if        %open(tivin00r)
166200070329     C                   close     tivin00r
166300070329     C                   endif
166400070329     C                   if        %open(fivabwwr)
166500070329     C                   close     fivabwwr
166600070329     C                   endif
166700070329     C                   if        %open(fivatwwr)
166800070329     C                   close     fivatwwr
166900070329     C                   endif
167000070326     C*
167100070326     C* Effettuo la chiamata al CLLE preposto
167200070326     C                   call(e)   'TITVVTC'
167300070326     C                   parm                    parccm
167400070326     C                   parm                    parmbr
167500070326     C                   parm      '2'           paropz
167600070326     C*
167700070326     C                   eval      wrkesito = '2'
167800070404     C*
167900070404     C                   seton                                        LR
168000070326     C*
168100070326     C                   ENDSR     '*CANCL'
168200070326     C***
168300080617
168400080617
168500080617
168600080617      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
168700080617     C     exeBon        BEGSR
168800080617     C*
168900080617     C* Imposto i parametri x il *pgm d "bonifica"
169000080617     C                   EVAL      ElencoChar = '1234567890'
169100080617     C                   EVAL      TipoElenco = *blanks
169200080617     C                   EVAL      CharSost   = *blanks
169300080617     C                   EVAL      UpperCase  = *blanks
169400080617     C                   EVAL      ChkNull    = '1'
169500080617     C                   EVAL      CharNull   = *blanks
169600080617     C*
169700100607     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VABRMN
169800080617     C                   EVAL      EsitoBon   = *blanks
169900080617     C*
170000080617     C                   CALL      'XCHKCHAR'
170100080617     C                   PARM                    TxtInOut
170200080617     C                   PARM                    ElencoChar
170300080617     C                   PARM                    TipoElenco
170400080617     C                   PARM                    CharSost
170500080617     C                   PARM                    UpperCase
170600080617     C                   PARM                    ChkNull
170700080617     C                   PARM                    CharNull
170800080617     C                   PARM                    EsitoBon
170900080617     C*
171000080617     C                   ENDSR
171100070326
171200070326
171300010330
171400010330
171500000613     C     *inzsr        BEGSR
171600990910     C*
171700990910     C     *entry        plist
171800990920     C                   parm                    tivlrds
171900990921     C                   parm      wrkesito      esito
172000000724     C                   parm                    prmlit
172100000710     C                   parm                    prmfir
172200010330     C*
172300010330     C* CALCOLA LA DATA CORRENTE
172400010330     C                   time                    wn14             14 0
172500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
172600010330     C                   z-add     wn8           g08dat
172700010330     C                   z-add     *zeros        g08inv
172800010330     C                   movel     '0'           g08err
172900010330     C                   call      'XSRDA8'
173000010330     C                   parm                    wlbda8
173100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
173200000613     C*
173300000613     C                   ENDSR
173400000613     C***
173500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
173600121107;W_VABCBO;VABNCL;VABPKB;VABCAS;FILL02;VABRMA;FILL03;VATNOT_E;
173700110314VABNT2;W_VABFFD;W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;VABNOT;W_VABRMN
173800080125
