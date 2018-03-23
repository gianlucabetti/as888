000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800080626     Ftitv1h5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100080626     Ftitv1h5ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070329     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040128     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040510     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500080125     D wFlgCAS         s              1    INZ(*blanks)
004600080626     D depvindta       s                   LIKE(vindta) INZ
004700030715     D*------------------
004800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900030715     D*------------------
005000030715     D SkSplitFLD      S             10    DIM(100)
005100030715     D SkSplitCSV      S            256    DIM(100)
005200030715     D CharCSV         S              1
005300030715     D CharTXT         S              1
005400030715     D CharNUM         S              1
005500030715     D posDa           S              3  0 INZ(*zeros)
005600030715     D posA            S              3  0 INZ(*zeros)
005700030715     D i               s              3  0 INZ(1)
005800030715     D wGiro           s              1  0 INZ(*zeros)
005900080125     D wVATNOT_A       s                   LIKE(VATNOT)
006000080125     D wVATNOT_B       s                   LIKE(VATNOT)
006100080125     D wVATNOT_E       s                   LIKE(VATNOT)
006200030822     D*------------------
006300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006400030822     D*------------------
006500030822     D posDaDft        S              3  0 INZ(*zeros)
006600030822     D posADft         S              3  0 INZ(*zeros)
006700030822     D j               s              3  0 INZ(1)
006800030822     D wGiroDft        s              1  0 INZ(*zeros)
006900010330     D*------------------
007000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007100010330     D*------------------
007200010330     D WLBDA8          DS                  INZ
007300010330     D  G08DAT                 1      8  0
007400010330     D  G08INV                 9     16  0
007500010330     D  G08ERR                17     17
007600010330     D  G08TGI                18     22  0
007700080625     D*------------------
007800080625     D* DS CALCOLO CHECK-DIGITS MOD103 (TRUL28R4)
007900080625     D*------------------
008000090916     D TRUL28DSX     E DS
008100031201     D*------------------
008200031201     D* Costanti
008300031201     D*------------------
008400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008600080617     D*------------------
008700080617     D* VARIABILI X "XCHKCHAR"
008800080617     D*------------------
008900080617     D TxtInOut        S           2048
009000080617     D ElencoChar      S            256
009100080617     D TipoElenco      S              1
009200080617     D CharSost        S              1
009300080617     D UpperCase       S              1
009400080617     D ChkNull         S              1
009500080617     D CharNull        S              1
009600080617     D EsitoBon        S              1
009700080617     D*------------------
009800080617     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
009900080617     D*------------------
010000080617     D PiStr           S            100A   INZ
010100080617     D PiDecChr        S              1A   INZ
010200080617     D PiVal           S             63S30 INZ
010300080617     D PiInt           S               N   INZ
010400080617     D PiNum           S               N   INZ
010500080617
010600080617     D*------------------
010700080617     D* LINKING A DEFINIZIONI ESTERNE
010800080617     D*------------------
010900080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
011000010330
011100010330
011200010330
011300990915     C                   time                    wrkdata
011400990915     C                   time                    wrkora
011500000913     C                   reset                   rrnum
011600990921     C                   reset                   esito
011700990921     C                   reset                   wrkesito
011800010601     C*
011900010601     C                   exsr      opeini
012000010605     C                   exsr      rwvab
012100010601     C*
012200010601     C                   seton                                        lr
012300010601
012400010601
012500010601
012600010601     C*--------------------------------------------------------
012700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012800010601     C*--------------------------------------------------------
012900010601     C     PREELA        BEGSR
013000010601     C*
013100010601     C* SE OCCORRE SPEDIRE IN FILIALE
013200010601     C                   if        invfil <> *zeros and
013300010601     C                             flgGiro = '0'
013400010601     C*
013500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013600010601     C                   eval      flgGiro = '1'
013700000724     C*
013800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014000010601     C     invfil        chain     azorg01l
014100000724     C                   if        %found
014200000724     C                   movel(p)  CMD(1)        depcmd
014300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
014400000724     C*
014500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014600011113     C                   Z-ADD     150           LENGH            15 5
014700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014800000724     C                   PARM                    depcmd
014900000724     C                   PARM                    LENGH
015000000724     C*
015100000724     C                   endif
015200000616     C*
015300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015400000616     C                   movel(p)  CMD(2)        depcmd
015500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015600000616     C*
015700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015800011113     C                   Z-ADD     150           LENGH            15 5
015900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016000000616     C                   PARM                    depcmd
016100000616     C                   PARM                    LENGH
016200000613     C*
016300080626     C                   if        not %open(titv1h5ps)
016400080626     C                   open      titv1h5ps
016500070329     C                   except    testdett
016600000613     C                   endif
016700010601     C*
016800010601     C                   endif
016900010601     C*
017000010601     C                   ENDSR
017100010601     C***
017200010601
017300010601
017400010601
017500010601
017600010601     C*--------------------------------------------------------
017700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017800010601     C*--------------------------------------------------------
017900010601     C     ENDELA        BEGSR
018000990915     C*
018100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
018200040119     C*
018300040119     C* Effettuo la chiamata al CLLE preposto
018400040119     C                   call(e)   'TITVVTC'
018500040510     C                   parm                    parccm
018600040119     C                   parm                    parmbr
018700040119     C                   parm      '2'           paropz
018800050201     C*
018900050201     C* Effettuo lancio TISI95 solo x chiusura
019000050201     C                   CLEAR                   TISI95DS
019100050201     C                   EVAL      I95TLA = 'C'
019200050201     C                   CALL      'TISI95R'
019300050201     C                   PARM                    TISI95DS
019400000613     C*
019500080626     C                   if        %open(titv1h5ps)
019600000616     C                   except    findett
019700080626     C                   close     titv1h5ps
019800000613     C                   endif
019900000616     C*
020000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020100011113     C                   Z-ADD     150           LENGH            15 5
020200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020300000616     C                   PARM                    CMD(3)
020400000616     C                   PARM                    LENGH
020500000616     C*
020600010601     C                   ENDSR
020700010601     C***
020800010601
020900010601
021000010601
021100000613
021200000613     C*--------------------------------------------------------
021300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021400000613     C*--------------------------------------------------------
021500000621     C     STPR          BEGSR
021600000613     C*
021700080626     C                   if        not %open(titv1h5p)
021800080626     C   50              open      titv1h5p
021900990915     C                   endif
022000990915     C*
022100030715     C   50              except    riepilogo
022200990915     C*
022300080626     C                   if        %open(titv1h5p)
022400080626     C   50              close     titv1h5p
022500990914     C                   endif
022600000613     C*
022700000613     C                   ENDSR
022800000613     C***
022900000613
023000010601
023100010601
023200010330     C*--------------------------------------------------------
023300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023400010330     C*--------------------------------------------------------
023500010605     C     RWVAB         BEGSR
023600010330     C*
023700010330     C                   if        not %open(tivin00r)
023800010330     C                   open      tivin00r
023900010330     C                   endif
024000021025     C                   if        not %open(fivabwwr)
024100021025     C                   open      fivabwwr
024200010330     C                   endif
024300040119     C*
024400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024500040119     C                   exsr      prevat
024600040119     C*
024700040119     C                   if        chkcall = '0'
024800040119     C*
024900040119     C                   if        not %open(fivatwwr)
025000040119     C                   open      fivatwwr
025100040119     C                   endif
025200010330     C*
025300040119     C                   clear                   §CTROKVB          7 0
025400040119     C                   clear                   §CTROKVT          7 0
025500040119     C                   clear                   §CTRMO            7 0
025600040119     C                   clear                   §CTRNO            7 0
025700070823     C*
025800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
026000080626     C                   exsr      inzvar
026100080626     C                   exsr      impvab
026200010330     C*
026300030822     C                   do        *HIVAL
026400010330     C*
026500030822     C                   read      tivin00r                               70
026600080626     C*
026700080626 XXX C* Forzature specifiche cliente corrente
026800080626     C                   exsr      exeforz
026900010618     C*
027000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027100010618     C                   if        vinflg = '1'
027200010618     C                   eval      flgOk = '1'
027300010618     C                   endif
027400040510     C*
027500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027600040510     C                   z-add     *zeros        wLenVINDTA        4 0
027700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
028000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
028100040510     C                   z-add     1             wFlgVINDTA
028200040510     C                   leave
028300040510     C                   endif
028400040510     C                   enddo
028500010618     C*
028600040510     C                   if        vindta > *blanks AND
028700040510     C                             wFlgVINDTA = 1
028800010330     C                   add       1             rrnum
028900010330     C*
029000010601     C                   if        *in70 = *off and
029100010330     C                             (vinflg = *blanks
029200010330     C                              or vinflg = '0'
029300010330     C                              or vinflg = '2')
029400010330     C*
029500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029600010711     C                   if        vinflg = *blanks or vinflg = '0'
029700010711     C                   clear                   vinmsg
029800010711     C                   endif
029900010601     C*
030000070925     C* Aggiungo un byte a blanks all'inizio d ogni record
030100070925     C                   eval      vindta  = ' ' + %trim(vindta)
030200070925     C*
030300010330     C                   exsr      inzvar
030400010605     C                   exsr      impvab
030500010601     C*
030600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030700010605     C                   eval      depfil = VABLNP
030800010601     C                   exsr      repfil
030900010601     C                   if        depfil = invfil
031000021025     C                   if        vlrpoi = 999
031100030822     C                   move(P)   invfil        VABFGS
031200021025     C                   else
031300030822     C                   move(P)   vlrpoi        VABFGS
031400021025     C                   endif
031500010601     C*
031600010601     C                   exsr      PREELA
031700010601     C*
031800010604     C* Ebbene...
031900010604     C*
032000040119     C  N31              add       1             §CTROKVB
032100040119     C   32              add       1             §CTRMO
032200040119     C   31              add       1             §CTRNO
032300030822     C                   if        wGiro = 2
032400040510     C* Scrivo sempre il VAT
032500040510     C  N31              exsr      wrivat
032600040510     C* Scrivo il VAB solo a rottura d spedizione
032700040119     C                   if        depspe =  *zeros OR
032800040119     C                             depspe <> vabNSP
032900040119     C  N31              exsr      wrivab
033000040119     C  N31              eval      depspe = vabNSP
033100040119     C                   endif
033200040119     C                   endif
033300010604     C*
033400010604     C                   if        *in31 = *off and
033500010604     C                             *in32 = *off
033600010604     C                   eval      vinflg = '1'
033700010604     C                   else
033800010604     C                   eval      recko = vindta
033900010604     C                   except    rigadett
034000010604     C                   eval      vinflg = '2'
034100010604     C                   endif
034200010604     C                   endif
034300010601     C*
034400010604     C                   endif
034500010604     C*
034600010330     C                   else
034700010330     C                   eval      vinflg = '1'
034800010330     C                   endif
034900010601     C*
035000080626     C                   eval      vindta = depvindta
035100010601     C  N70              update    tivin000
035200010330     C*
035300030822     C  N70              enddo
035400040119     C*
035500040119     C                   endif                                                  (endif - chkcall)
035600010601     C*
035700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035800010601     C                   if        cntNonEl = *zeros or
035900010601     C                             flgMulti = '0'
036000010330     C* Se non ci sono record con errori ...
036100010601     C                   if        §ctrno = 0 and
036200010604     C                             §ctrmo = 0 and
036300010601     C                             flgStato <> '2'
036400010330     C* ... restituisco esito OK.
036500010330     C                   eval      wrkesito = '0'
036600010330     C                   else
036700040119     C                   if        §ctrokvb > 0
036800010330     C                   eval      wrkesito = '1'
036900010330     C                   else
037000010615     C                   if        flgOk = '0'
037100010615     C                   eval      wrkesito = '2'
037200010615     C                   else
037300010615     C                   eval      wrkesito = '6'
037400010615     C                   endif
037500010330     C                   endif
037600010330     C                   endif
037700010601     C                   else
037800010601     C                   eval      wrkesito = '9'
037900010601     C                   endif
038000010330     C*
038100010330     C                   if        %open(tivin00r)
038200010330     C                   close     tivin00r
038300010330     C                   endif
038400021025     C                   if        %open(fivabwwr)
038500021025     C                   close     fivabwwr
038600010330     C                   endif
038700040512     C                   if        %open(fivatwwr)
038800040512     C                   close     fivatwwr
038900040512     C                   endif
039000010601     C*
039100010601     C                   if        vlrpoi <> 999
039200010601     C                   eval      invfil = vlrpoi
039300010601     C                   endif
039400010330     C*
039500040119     C                   if        §ctrokvb > 0
039600010601     C                             and invfil > *zeros
039700010330     C                   exsr      invio
039800010330     C                   endif
039900010601     C*
040000010618     C                   if        flgGiro = '1'
040100010601     C                   exsr      endela
040200010618     C                   endif
040300010330     C*
040400010330     C                   ENDSR
040500010330     C***
040600040119
040700040119
040800040119
040900040119     C*----------------------------------------------------*
041000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
041100040119     C*----------------------------------------------------*
041200040510     C     WRIVAB        BEGSR
041300040119     C*
041400060331     C* Gestisco l'eventuale rottura x numero spedizione
041500070823     C                   if        VABNSP <> depspe
041600070301     C                   if        wVATNOT_E <> *blanks
041700080125     C                   if        VABCTM = *blanks
041800080125     C                   eval      VABCTM = '7Q'
041900080125     C                   endif
042000070301     C                   endif
042100040119     C                   write     fivab000                                     => scarico il VAB
042200060331     C                   endif
042300040119     C*
042400040119     C                   ENDSR
042500040119
042600040119
042700040119
042800040119     C*----------------------------------------------------*
042900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
043000040119     C*----------------------------------------------------*
043100040510     C     WRIVAT        BEGSR
043200060512     C*
043300060512     C* Inizializzo i campi chiave
043400060512     C                   eval      VATFGS = VABFGS
043500060512     C                   eval      VATAAS = VABAAS
043600060512     C                   eval      VATCCM = VABCCM
043700070823     C                   eval      VATNRS = VABNRS
043800060512     C                   eval      VATNSP = VABNSP
043900060512     C                   eval      VATLNP = VABLNP
044000060331     C*
044100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
044200060331     C                   if        wVATNOT_A <> *blanks
044300060331     C                   eval      VATNOT = wVATNOT_A
044400060331     C                   eval      VATTRC = 'A'
044500060331     C                   write     FIVAT000
044600060331     C                   add       1             §CTROKVT
044700060331     C                   endif
044800060331     C*
044900060331     C                   if        wVATNOT_B <> *blanks
045000060331     C                   eval      VATNOT = wVATNOT_B
045100060331     C                   eval      VATTRC = 'B'
045200060331     C                   write     FIVAT000
045300060331     C                   add       1             §CTROKVT
045400060331     C                   endif
045500060331     C*
045600080625     C                   if        wVATNOT_E <> *blanks
045700060331     C                   eval      VATNOT = wVATNOT_E
045800060331     C                   eval      VATTRC = 'E'
045900080625     C*
046000080625     C                   z-add     *zeros        wBarcodeDA       11 0
046100080625     C                   z-add     *zeros        wBarcodeA        11 0
046200080711     C                   if        %len(%trim(wVATNOT_E)) = 14
046300080625     C                   eval      PiStr=%subst(%trim(wVATNOT_E):3:11)
046400080625     C                   exsr      CHKNUM
046500080625     C                   if        PiInt=*on
046600080625     C*
046700080625     C                   z-add     PiVal         wBarcodeDA
046800080625     C                   eval      wBarcodeA = wBarcodeDA + VABNCL - 1
046900080625     C*
047000080625     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
047100080625     C     wBarcodeDA    DO        wBarcodeA     wBarcodeNum      11 0
047200080625     C*
047300080625     C* Compongo la parte variabile
047400080625     C                   MOVE(P)   wBarcodeNum   wBarcodeAlfa     11
047500080625     C* Eseguo calcolo check-digit
047600090916     C                   CLEAR                   TRUL28DSX
047700090916     C                   EVAL      I284STD = 'E10'
047800080625     C                   EVAL      I284COD = wBarcodeAlfa
047900080625     C                   CALL      'TRUL28R4'
048000090916     C                   PARM                    TRUL28DSX
048100080625     C* Se tutto ok scarico il buffer del FNVAT
048200080625     C                   IF        O284ERR <> '1'
048300080625     C                   EVAL      VATNOT = 'AA'+%trim(O284COD)
048400080625     C                   WRITE     FIVAT000
048500080625     C                   ADD       1             §CTROKVT
048600080625     C                   ELSE
048700080625     C                   SETON                                        32
048800080625     C                   EVAL      vinmsg = %trimr(vinmsg)
048900080625     C                             + ' ' + 'VATNOT'
049000080625     C                   ENDIF
049100080625     C                   ENDDO
049200080625     C*
049300080625     C                   seton                                        32
049400080625     C                   eval      vinmsg = %trimr(vinmsg)
049500080625     C                             + ' ' + 'VATNOT_E'
049600080625     C                   endif
049700060331     C                   endif
049800080625     C                   endif
049900040119     C*
050000040119     C                   ENDSR
050100040119
050200040119
050300040119
050400040119     C*----------------------------------------------------*
050500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050600040119     C*----------------------------------------------------*
050700040119     C     PREVAT        BEGSR
050800040119     C*
050900040119     C* Compongo il nome del membro da dare al FIVATWWR
051000040119     C                   eval      parmbr = vlrhdl
051100040119     C                   movel     'M'           parmbr
051200040510     C                   eval      parccm = vlrksc
051300040119     C                   eval      paropz = '1'
051400040119     C* Effettuo la chiamata al CLLE preposto
051500040119     C                   call(e)   'TITVVTC'
051600040119     C                   parm                    parccm
051700040119     C                   parm                    parmbr
051800040119     C                   parm                    paropz
051900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052000040119     C                   if        %error
052100040119     C                   movel     '1'           chkcall
052200040119     C                   else
052300040119     C                   movel     '0'           chkcall
052400040119     C                   endif
052500040119     C*
052600040119     C                   ENDSR
052700010601
052800010601
052900010601
053000010330     C*----------------------------------------------------*
053100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053200010330     C*----------------------------------------------------*
053300040119     C     INZVAR        BEGSR
053400010330     C*
053500030822     C* Inizializzo variabili di wrk
053600020204     C                   Z-ADD     *zeros        Num5_0            5 0
053700060331     C                   MOVEL     *blanks       wVATNOT_A
053800060331     C                   MOVEL     *blanks       wVATNOT_B
053900060331     C                   MOVEL     *blanks       wVATNOT_E
054000080617     C                   EVAL      wFlgCAS = *blanks
054100030822     C*
054200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054300030822     C                   CLEAR                   FIVAB000
054400040119     C                   CLEAR                   FIVAT000
054500030822     C                   CLEAR                   SkSplitCSV
054600030822     C                   EVAL      i = 1
054700030822     C                   EVAL      posDa = *zeros
054800030822     C                   EVAL      posA  = *zeros
054900030822     C*
055000030822     C* Reimposto i valori di default
055100030822     C                   EXSR      DEFCAM
055200030822     C*
055300010330     C                   ENDSR
055400010330     C*----------------------------------------------------*
055500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055600010330     C*----------------------------------------------------*
055700010330     C     DEFCAM        BEGSR
055800030822     C*
055900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056000030715     C* e delimitatore testo.
056100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056400030822     C*
056500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
056700030715     C                   SETON                                        50
056800030715     C                   ELSE
056900030715     C                   SETOFF                                       50
057000030715     C                   ENDIF
057100030822     C*
057200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057300030822     C                   EVAL      posDaDft = 1
057400030822     C                   EVAL      posADft  = 0
057500030822     C                   EVAL      wGiroDft = 0
057600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057700030822     C                             posDaDft > 0
057800030822     C*
057900030822     C* Gestisco il 1° giro
058000030822     C                   IF        wGiroDft = 0
058100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058300030822     C* Incremento il contatore dei "giri"
058400030822     C                   EVAL      wGiroDft = 1
058500030822     C                   ELSE
058600030822     C                   EVAL      posDaDft = posADft
058700030822     C                   ENDIF
058800030822     C* Eseguo lo scan x trovare la fine del campo corrente
058900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059000030822     C*
059100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059300030822     C                   IF        posDaDft > 0 AND
059400030822     C                             posADft  > 0
059500030822     C* NCL
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'NCL'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500030822     C                   Z-ADD     PiVal         VABNCL
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* CCM
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'CCM'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiInt=*on
061800030822     C                   Z-ADD     PiVal         VABCCM
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* LNP
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'LNP'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiInt=*on
063100030822     C                   Z-ADD     PiVal         VABLNP
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* NRS
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'NRS'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiInt=*on
064400030822     C                   Z-ADD     PiVal         VABNRS
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* CTR
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'CTR'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiInt=*on
065700030822     C                   Z-ADD     PiVal         VABCTR
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* PKB
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'PKB'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiNum=*on
067000030822     C                   Z-ADD     PiVal         VABPKB
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* VLB
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'VLB'
067800030822     C                   EVAL      PiStr=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   EXSR      CHKNUM
068200030822     C                   IF        PiNum=*on
068300030822     C                   Z-ADD     PiVal         VABVLB
068400030822     C                   ENDIF
068500030822     C                   ENDIF
068600030822     C* QFT
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'QFT'
069100030822     C                   EVAL      PiStr=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   EXSR      CHKNUM
069500030822     C                   IF        PiNum=*on
069600030822     C                   Z-ADD     PiVal         VABQFT
069700030822     C                   ENDIF
069800030822     C                   ENDIF
069900030822     C* CBO
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'CBO'
070400030822     C                   EVAL      VABCBO=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   ENDIF
070800030822     C* TSP
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'TSP'
071300030822     C                   EVAL      VABTSP=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   ENDIF
071700030822     C* VAS
071800030822     C                   IF        %subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):1:3)
072100030822     C                             = 'VAS'
072200030822     C                   EVAL      VABVAS=%trim(%subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500030822     C                   ENDIF
072600030822     C* VCA
072700030822     C                   IF        %subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):1:3)
073000030822     C                             = 'VCA'
073100030822     C                   EVAL      VABVCA=%trim(%subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073400030822     C                   ENDIF
073500070925     C* TIC
073600070925     C                   IF        %subst(
073700070925     C                             %subst(vlrppt:posDaDft+1:
073800070925     C                             posADft-posDaDft-1):1:3)
073900070925     C                             = 'TIC'
074000070925     C                   EVAL      VABTIC=%trim(%subst(
074100070925     C                             %subst(vlrppt:posDaDft+1:
074200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300070925     C                   ENDIF
074400030822     C* GCA
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'GCA'
074900030822     C                   EVAL      VABGCA=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300030822     C* CTM
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'CTM'
075800030822     C                   EVAL      VABCTM=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200030822     C* FFD
076300030822     C                   IF        %subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):1:3)
076600030822     C                             = 'FFD'
076700030822     C                   EVAL      VABFFD=%trim(%subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000030822     C                   ENDIF
077100030822     C* VAD
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'VAD'
077600030822     C                   EVAL      VABVAD=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000030822     C* GMA
078100030822     C                   IF        %subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):1:3)
078400030822     C                             = 'GMA'
078500030822     C                   EVAL      VABGMA=%trim(%subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800030822     C                   ENDIF
078900030822     C* GGA
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'GGA'
079400030822     C                   EVAL      VABGGA=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* GVA
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'GVA'
080300030822     C                   EVAL      VABGVA=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700030822     C* TC1
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100030822     C                             = 'TC1'
081200030822     C                   EVAL      VABTC1=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600030822     C* TC2
081700030822     C                   IF        %subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):1:3)
082000030822     C                             = 'TC2'
082100030822     C                   EVAL      VABTC2=%trim(%subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400030822     C                   ENDIF
082500040714     C* VATTRC
082600040714     C                   IF        %subst(
082700040714     C                             %subst(vlrppt:posDaDft+1:
082800040714     C                             posADft-posDaDft-1):1:3)
082900040714     C                             = 'TRC'
083000040714     C                   EVAL      VATTRC=%trim(%subst(
083100040714     C                             %subst(vlrppt:posDaDft+1:
083200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083300040714     C                   ENDIF
083400030822     C* ...
083500030822     C                   ENDIF
083600030822     C                   ENDDO
083700020204     C*
083800010330     C                   ENDSR
083900010607     C*----------------------------------------------------*
084000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
084100010607     C*----------------------------------------------------*
084200010607     C     IMPVAB        BEGSR
084300010607     C*
084400010607     C                   SETOFF                                       3132
084500030515     C*
084600010607     C                   MOVEL     datcor        VABAAS
084700010607     C                   MOVE      datcor        VABMGS
084800010607     C*
084900030715     C**********
085000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085100030715     C**********
085200030715     C                   IF        wGiro = *zeros
085300040113     C*
085400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085600040510     C                             %trim(intcol(3))<>*blanks
085700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085800040113     C                                      %trim(intcol(3))
085900070823     C     ';':CharCSV   XLATE     vindta        vindta
086000040510     C                   ENDIF
086100080626     C*
086200080626 XXX C* Forzature specifiche cliente corrente
086300080626     C                   exsr      exeforz
086400080626     C*
086500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086600031201     C     minu:maiu     XLATE     vindta        vindta
086700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086800030715     C                   DOW       posDa <= %len(%trim(vindta))
086900030715     C*
087000030715     C* Gestisco il 1° campo
087100030715     C                   IF        i = 1
087200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087300030715     C                   EVAL      posDa = 1
087400030715     C* Eseguo lo scan x trovare la fine del primo campo
087500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C* X i campi successivi al 1°
088000030715     C                   ELSE
088100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088200030715     C                   EVAL      posDa = posA + 1
088300030715     C* Eseguo lo scan x trovare la fine del campo corrente
088400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088500030715     C* Gestisco l'ultimo campo
088600030715     C                   IF        posA = *zeros
088700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   LEAVE
089200030715     C                   ELSE
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C                   ENDIF
089700030715     C                   ENDIF
089800030715     C* Incremento il contatore d campo
089900030715     C                   EVAL      i = i +1
090000030715     C                   ENDDO
090100030715     C*
090200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090300030715     C* delimitazione testo
090400030715     C                   EVAL      i = 1
090500030715     C                   DOW       i < %elem(SkSplitFLD)
090600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090800030715     C                   EVAL      i = i + 1
090900030715     C                   ENDDO
091000030715     C                   EVAL      wGiro = 1
091100030715     C                   ELSE
091200030715     C                   EVAL      wGiro = 2
091300031204     C**********
091400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091500031204     C* d separatore campo
091600031204     C**********
091700031204     C                   EVAL      vindta = %trim(vindta)
091800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091900031204     C                   EVAL      vindta = %subst(vindta:2)
092000031204     C                   ENDDO
092100031204     C*
092200031204     C                   Z-ADD     *zeros        lunghInput        4 0
092300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092700031204     C                   ENDDO
092800030715     C**********
092900030715     C* Effettuo lo split del campo dati d input
093000030715     C**********
093100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093200030715     C                   DOW       posDa <= %len(%trim(vindta))
093300030715     C*
093400030715     C* Gestisco il 1° campo
093500030715     C                   IF        i = 1
093600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093700030715     C                   EVAL      posDa = 1
093800030715     C* Eseguo lo scan x trovare la fine del primo campo
093900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094000030715     C* A questo punto "estraggo" il campo corrente
094100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094200030715     C                                                    (posA-posDa))
094300030715     C* X i campi successivi al 1°
094400030715     C                   ELSE
094500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094600030715     C                   EVAL      posDa = posA + 1
094700031204     C* Verifico che nn vi sia il campo nn valorizzato
094800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094900031204     C* Se campo nn valorizzato skippo al prossimo
095000031204     C                   EVAL      SkSplitCSV(i) = *blanks
095100031204     C                   EVAL      posA  = posA + 1
095200031204     C                   ELSE
095300030715     C* Eseguo lo scan x trovare la fine del campo corrente
095400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095500030715     C* Gestisco l'ultimo campo
095600030715     C                   IF        posA = *zeros
095700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   LEAVE
096200030715     C                   ELSE
096300030715     C* A questo punto "estraggo" il campo corrente
096400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096500030715     C                                                    (posA-posDa))
096600030715     C                   ENDIF
096700031204     C                   ENDIF
096800030715     C                   ENDIF
096900030715     C* Incremento il contatore d campo
097000030715     C                   EVAL      i = i +1
097100030715     C                   ENDDO
097200030715     C*
097300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097400030715     C* delimitazione testo
097500030715     C                   EVAL      i = 1
097600030715     C                   DOW       i < %elem(SkSplitCSV)
097700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097900030715     C                   EVAL      i = i + 1
098000030715     C                   ENDDO
098100030715     C*
098200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098300030715     C                   EVAL      i = 1
098400030715     C                   DOW       i < %elem(SkSplitFLD)
098500030715     C*
098600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098800030715     C                             SkSplitCSV(i) <> *blanks
098900030721     C*** ATB
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099100030721     C                   EVAL      VABATB = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030715     C*** RSD
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099800030715     C* ==
099900030715     C                   ENDIF
100000030721     C*** RD2
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030715     C*** IND
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100600030715     C                   EVAL      VABIND = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** LOD
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** PRD
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030721     C*** NZD
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030715     C*** RMA
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** NOT
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** NT2
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** VCA
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030715     C*** VAS
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030715     C*** FFD
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030721     C*** GC1
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** GC2
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** TSP
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105500030721     C                   ENDIF
105600040510     C*** CBO
105700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105900040510     C                   ENDIF
106000030721     C*** NAS
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TIC
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GCA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** XCO
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** CTM
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TCR
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** CTS
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** FTM
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** VAD
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** GMA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** GGA
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** GVA
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** TC1
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** TC2
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** SCL
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** RMO
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** NMO
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*
112900030721     C* Reperisco quindi i campi numerici...
113000040510     C*** NSP
113100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113200040510     C                   EVAL      PiStr=SkSplitCSV(i)
113300040510     C                   EXSR      CHKNUM
113400040510     C                   IF        PiInt=*on
113500040510     C                   Z-ADD     PiVal         VABNSP
113600040510     C                   ELSE
113700040510     C                   SETON                                        31
113800040510     C                   EVAL      VABNSP = *zeros
113900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114000040510     C                             + ' ' + 'VABNSP'
114100040510     C                   ENDIF
114200040510     C                   ENDIF
114300071106     C*** RMN
114400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114500071106     C                   EVAL      PiStr=SkSplitCSV(i)
114600071106     C                   EXSR      CHKNUM
114700071106     C                   IF        PiInt=*on
114800071106     C                   Z-ADD     PiVal         VABRMN
114900071106     C                   ELSE
115000071106     C                   SETON                                        32
115100071106     C                   EVAL      VABRMN = *zeros
115200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115300071106     C                             + ' ' + 'VABRMN'
115400071106     C                   ENDIF
115500071106     C                   ENDIF
115600030721     C*** CAD
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on
116100030721     C                   Z-ADD     PiVal         Num5_0
116200030721     C                   MOVEL(p)  Num5_0        VABCAD
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABCAD = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABCAD'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** NCL
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on
117500030721     C                   Z-ADD     PiVal         VABNCL
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABNCL = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000040122     C                             + ' ' + 'VABNCL'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** PKB
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiNum=*on
118800030721     C                   Z-ADD     PiVal         VABPKB
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      VABPKB = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABPKB'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** CAS
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119800080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
119900080617     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
120000080627     C                             %trim(SkSplitCSV(i)) <> '0,00'   AND
120100080617     C                             %trim(SkSplitCSV(i)) <> '0'
120200080125     C                   EVAL      wFlgCAS = '1'
120300080125     C                   ENDIF
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiNum=*on
120700030721     C                   Z-ADD     PiVal         VABCAS
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      VABCAS = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABCAS'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** CCM
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on      AND
122000030721     C                             PiVal<=9999999 AND
122100030721     C                             PiVal>*zeros
122200030721     C                   Z-ADD     PiVal         VABCCM
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      VABCCM = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700040510     C                             + ' ' + 'VABCCM'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** LNP
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on    AND
123500030721     C                             PiVal<=999   AND
123600030721     C                             PiVal>*zeros
123700030721     C                   Z-ADD     PiVal         VABLNP
123800030721     C                   ELSE
123900040715     C                   SETON                                        32
124000030721     C                   EVAL      VABLNP = *zeros
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200040510     C                             + ' ' + 'VABLNP'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** NRS
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on AND
125000030721     C                             PiVal<=99
125100030721     C                   Z-ADD     PiVal         VABNRS
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABNRS'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** LNA
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on    AND
126300030721     C                             PiVal<=999   AND
126400030721     C                             PiVal>*zeros
126500030721     C                   Z-ADD     PiVal         VABLNA
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      VABLNA = *zeros
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABLNA'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** CTR
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on  AND
127800030721     C                             PiVal<=999
127900030721     C                   Z-ADD     PiVal         VABCTR
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABCTR'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** IAS
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiNum=*on
129100030721     C                   Z-ADD     PiVal         VABIAS
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABIAS'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** VLB
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiNum=*on
130300030721     C                   Z-ADD     PiVal         VABVLB
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABVLB'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** QFT
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiNum=*on
131500030721     C                   Z-ADD     PiVal         VABQFT
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABQFT'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** NCD
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on      AND
132700030721     C                             PiVal<=9999999
132800030721     C                   Z-ADD     PiVal         VABNCD
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABNCD'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** NCA
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on      AND
134000030721     C                             PiVal<=9999999
134100030721     C                   Z-ADD     PiVal         VABNCA
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABNCA'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** ZNC
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on AND
135300030721     C                             PiVal<=99
135400030721     C                   Z-ADD     PiVal         VABZNC
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABZNC'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** DCR
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on       AND
136600030721     C                             PiVal<=99999999
136700030721     C                   Z-ADD     PiVal         VABDCR
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABDCR'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** HCR
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiInt=*on   AND
137900030721     C                             PiVal<=9999
138000030721     C                   Z-ADD     PiVal         VABHCR
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABHCR'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700030721     C*** VMD
138800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138900030721     C                   EVAL      PiStr=SkSplitCSV(i)
139000030721     C                   EXSR      CHKNUM
139100030721     C                   IF        PiNum=*on
139200030721     C                   Z-ADD     PiVal         VABVMD
139300030721     C                   ELSE
139400030721     C                   SETON                                        32
139500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139600030721     C                             + ' ' + 'VABVMD'
139700030721     C                   ENDIF
139800030721     C                   ENDIF
139900030721     C*** ANT
140000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140100030721     C                   EVAL      PiStr=SkSplitCSV(i)
140200030721     C                   EXSR      CHKNUM
140300030721     C                   IF        PiInt=*on        AND
140400030721     C                             PiVal<=999999999
140500030721     C                   Z-ADD     PiVal         VABANT
140600030721     C                   ELSE
140700030721     C                   SETON                                        32
140800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140900030721     C                             + ' ' + 'VABANT'
141000030721     C                   ENDIF
141100030721     C                   ENDIF
141200030721     C*** CMO
141300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141400030721     C                   EVAL      PiStr=SkSplitCSV(i)
141500030721     C                   EXSR      CHKNUM
141600030721     C                   IF        PiInt=*on
141700030721     C                   Z-ADD     PiVal         Num5_0
141800030721     C                   MOVEL(p)  Num5_0        VABCMO
141900030721     C                   ELSE
142000030721     C                   SETON                                        32
142100030721     C                   EVAL      VABCMO = *zeros
142200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142300030721     C                             + ' ' + 'VABCMO'
142400030721     C                   ENDIF
142500030721     C                   ENDIF
142600040510     C*
142700040510     C* Reperisco i campi del FIVAT
142800040510     C*** VATNOT
142900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
143000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143100040510     C                   ENDIF
143200040510     C*** VATTRC
143300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143500040510     C                   ENDIF
143600060331     C*** VATNOT_A
143700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143900060331     C                   ENDIF
144000060331     C*** VATNOT_B
144100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144300060331     C                   ENDIF
144400060331     C*** VATNOT_E
144500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144700060331     C                   ENDIF
144800070823     C*
144900071029     C***  ===>  Gestione campi "particolari" dentro ciclo
145000071204     C*
145100080617     C*** NSP/RMN/RMA
145200080617     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
145300080617     C                   EVAL      VABRMA    = %trim(SkSplitCSV(i))
145400080617     C                   EVAL      TxtInOut  = VABRMA
145500080617     C                   EXSR      exeBon
145600080617     C                   EVAL      PiStr=TxtInOut
145700071204     C                   EXSR      CHKNUM
145800071204     C                   IF        PiInt=*on
145900071204     C                   Z-ADD     PiVal         VABNSP
146000071204     C                   Z-ADD     PiVal         VATNSP
146100071204     C                   Z-ADD     PiVal         VABRMN
146200071204     C                   ELSE
146300071204     C                   SETON                                        32
146400071204     C                   EVAL      VABNSP = *zeros
146500071204     C                   EVAL      VATNSP = *zeros
146600071204     C                   EVAL      VABRMN = *zeros
146700071204     C                   EVAL      vinmsg = %trimr(vinmsg)
146800071204     C                             + ' ' + 'VABNSP VATNSP VABRMN'
146900071204     C                   ENDIF
147000071204     C                   ENDIF
147100070925     C*
147200071029     C***  <===  -----------------------------------------
147300040510     C*
147400030715     C                   ENDIF
147500030715     C                   EVAL      i = i + 1
147600030715     C                   ENDDO
147700070925     C*
147800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147900080125     C*
148000070925     C*
148100071029     C***  <===  -----------------------------------------
148200070925     C*
148300070925     C* Considerazioni finali su CBO/CAS
148400080125     C                   IF        wFlgCAS = '1'
148500070925     C                   IF        VABCBO = '1'
148600070925     C                   EVAL      VABCBO = '4'
148700070925     C                   ENDIF
148800070925     C                   IF        VABCBO = '2'
148900070925     C                   EVAL      VABCBO = '6'
149000070925     C                   ENDIF
149100070925     C                   ENDIF
149200070925     C***  <===  ----------------------------
149300040510     C*
149400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149500040510     C                   IF        VABNSP = *zeros
149600040510     C                   SETON                                        31
149700040510     C                   ENDIF
149800040715     C*
149900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150000040715     C                   IF        VABLNP = *zeros
150100040715     C                   SETON                                        31
150200040715     C                   ENDIF
150300040510     C*
150400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150500040510     C                   IF        VABRMN = *zeros
150600040510     C                   Z-ADD     VABNSP        VABRMN
150700040510     C                   ENDIF
150800040510     C*
150900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151000040510     C                   IF        VABRMA = *blanks
151100040714     C                   MOVEL     VABRMN        VABRMA
151200040510     C                   ENDIF
151300040128     C*
151400040510     C* Se provincia nn valorizzata la reperisco
151500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151600040510     C                   IF        VABPRD = *blanks
151700040128     C                   CLEAR                   TISI95DS
151800040128     C                   EVAL      I95TCN = '3'
151900040128     C                   Z-ADD     datcor        I95DAT
152000040128     C                   EVAL      I95NAR = VABNZD
152100040128     C                   EVAL      I95CAP = VABCAD
152200040128     C                   EVAL      I95LOC = VABLOD
152300040128     C                   CALL      'TISI95R'
152400040128     C                   PARM                    TISI95DS
152500040128     C                   EVAL      VABPRD = O95PRV
152600040510     C                   ENDIF
152700020204     C*
152800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152900020204     C                   EXSR      CHKIMPDIV
153000030715     C                   ENDIF
153100020204     C*
153200010607     C                   ENDSR
153300010607     C*----------------------------------------------------*
153400010601
153500020204
153600020204     C*----------------------------------------------------*
153700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153800020204     C*----------------------------------------------------*
153900020204     C     CHKIMPDIV     BEGSR
154000020204     C*
154100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154200020204     C                   Z-ADD     *zeros        wrkDec            9 9
154300020204     C*
154400020204     C* Come prima cosa effettuo considerazioni sulla divisa
154500020204     C                   IF        vabIAS > *zeros
154600020204     C                   IF        vabVAS <> 'EUR'
154700020204     C                   EVAL      vabVAS =  'ITL'
154800020204     C                   ENDIF
154900020204     C                   ENDIF
155000020204     C*
155100020204     C                   IF        vabCAS > *zeros
155200020204     C                   IF        vabVCA <> 'EUR'
155300020204     C                   EVAL      vabVCA =  'ITL'
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C                   IF        vabVMD > *zeros
155800020204     C                   IF        vabVAD <> 'EUR'
155900020204     C                   EVAL      vabVAD =  'ITL'
156000020204     C                   ENDIF
156100020204     C                   ENDIF
156200020204     C*
156300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156400020204     C                   Z-ADD     vabIAS        wrkDec
156500020204     C                   IF        wrkDec > *zeros
156600020204     C                   IF        vabVAS = 'ITL'
156700020204     C                   EVAL      vabIAS = *zeros
156800020204     C                   ENDIF
156900020204     C                   ENDIF
157000020204     C*
157100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157200020204     C                   Z-ADD     vabCAS        wrkDec
157300020204     C                   IF        wrkDec > *zeros
157400020204     C                   IF        vabVCA = 'ITL'
157500020204     C                   EVAL      vabCAS = *zeros
157600020204     C                   ENDIF
157700020204     C                   ENDIF
157800020204     C*
157900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158000020204     C                   Z-ADD     vabVMD        wrkDec
158100020204     C                   IF        wrkDec > *zeros
158200020204     C                   IF        vabVAD = 'ITL'
158300020204     C                   EVAL      vabVMD = *zeros
158400020204     C                   ENDIF
158500020204     C                   ENDIF
158600020204     C*
158700020204     C                   ENDSR
158800020204     C***
158900080617
159000080617
159100080617
159200080617     C*----------------------------------------------------*
159300080617     C*  CONTROLLO NUMERICITA' CAMPI
159400080617     C*----------------------------------------------------*
159500080617     C     CHKNUM        BEGSR
159600080617     C*
159700080617     C                   IF        PiDecChr = *blanks
159800080617     C                   EVAL      PiDecChr = CharNUM
159900080617     C                   ENDIF
160000080617     C*
160100080617     C                   callp     UBISNUM_Check(PiStr
160200080617     C                                          :PiDecChr
160300080617     C                                          :PiVal
160400080617     C                                          :PiNum
160500080617     C                                          :PiInt)
160600080617     C*
160700080617     C                   ENDSR
160800080617     C***
160900010330
161000010601
161100010601
161200010601
161300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161400010601     C     repfil        BEGSR
161500010601     C*
161600010601     C                   if        invfil = *zeros and
161700010601     C                             depfil > *zeros and
161800010629     C                             (vinflg = *blanks or
161900010629     C                              vinflg = *zeros)
162000010601     C
162100010601     C                   eval      invfil = depfil
162200010601     C                   endif
162300010601     C*
162400010601     C                   if        depfil <> invfil and
162500010601     C                             invfil > *zeros
162600010601     C                   eval      flgMulti = '1'
162700010601     C                   if        vinflg = *blanks
162800010601     C                   add       1             cntNonEl
162900010601     C                   endif
163000010601     C                   endif
163100010601     C*
163200010601     C                   if        vinflg = '2'
163300010601     C                   eval      flgStato = '2'
163400010601     C                   endif
163500010601     C*
163600010601     C                   ENDSR
163700010601     C***
163800010601
163900010601
164000010601
164100010330
164200040119      /TITLE Invio dei dati al punto operativo.
164300040119     C     invio         BEGSR
164400040119     C*
164500040119     C* 1° invio FIVAT
164600040119     C                   reset                   dscmz
164700040119     C                   move      vlrpoi        cmzdst
164800040119     C                   eval      cmzfld = 'FIVATWWR'
164900040119     C                   eval      cmzmbd = vlrhdl
165000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165100040119     C***                if        prmfir = *blanks
165200040119     C                   eval      cmzfla = 'FIVAT00F'
165300040119     C                   eval      cmzmba = 'FIVAT00F'
165400040119     C***                else
165500040119     C***                eval      cmzfla = prmfir
165600040119     C***                eval      cmzmba = prmfir
165700040119     C***                endif
165800040119     C                   eval      cmznrr = *zeros
165900040119     C                   move      §ctrokvt      cmznrr
166000040119     C                   eval      cmzlba = vlrfl1
166100040119     C                   call(e)   'TIS711C'
166200040119     C                   parm                    dscmz
166300040119     C                   parm      *blanks       esito
166400040119     C                   if        %error
166500040119     C                             or cmzerr = '1'
166600040119     C                             or esito  = '1'
166700040119     C                   eval      wrkesito = '3'
166800040119     C                   else
166900040119     C*
167000040119     C* 2° invio FIVAB
167100040119     C                   reset                   dscmz
167200040119     C                   move      vlrpoi        cmzdst
167300040119     C                   eval      cmzfld = vlrfou
167400040119     C                   eval      cmzmbd = vlrhdl
167500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167600040119     C***                if        prmfir = *blanks
167700040119     C                   eval      cmzfla = 'FIVAB00F'
167800040119     C                   eval      cmzmba = 'FIVAB00F'
167900040119     C***                else
168000040119     C***                eval      cmzfla = prmfir
168100040119     C***                eval      cmzmba = prmfir
168200040119     C***                endif
168300040119     C                   eval      cmznrr = *zeros
168400040119     C                   move      §ctrokvb      cmznrr
168500040119     C                   eval      cmzlba = vlrfl1
168600040119     C                   call(e)   'TIS711C'
168700040119     C                   parm                    dscmz
168800040119     C                   parm      *blanks       esito
168900040119     C                   if        %error
169000040119     C                             or cmzerr = '1'
169100040119     C                             or esito  = '1'
169200040119     C                   eval      wrkesito = '3'
169300040119     C                   endif
169400040119     C                   endif
169500040119     C*
169600040119     C                   ENDSR
169700040119     C***
169800010601
169900010601
170000010601
170100010601
170200010601      /TITLE Invio dei dati al punto operativo.
170300010601     C     opeini        BEGSR
170400010601     C*
170500010601     C* Inizializzo flag e contatori operativi
170600010601     C                   movel     '0'           flgGiro           1
170700010601     C                   movel     '0'           flgMulti          1
170800010601     C                   movel     '1'           flgStato          1
170900010615     C                   movel     '0'           flgOk             1
171000010601     C                   z-add     *zeros        cntNonEl         10 0
171100010601     C                   z-add     *zeros        depfil            3 0
171200010601     C                   z-add     *zeros        invfil            3 0
171300010601     C*
171400010601     C                   ENDSR
171500010601     C***
171600070326
171700070326
171800070326
171900070326
172000070326     C     *pssr         BEGSR
172100070329     C*
172200070329     C                   if        %open(tivin00r)
172300070329     C                   close     tivin00r
172400070329     C                   endif
172500070329     C                   if        %open(fivabwwr)
172600070329     C                   close     fivabwwr
172700070329     C                   endif
172800070329     C                   if        %open(fivatwwr)
172900070329     C                   close     fivatwwr
173000070329     C                   endif
173100070326     C*
173200070326     C* Effettuo la chiamata al CLLE preposto
173300070326     C                   call(e)   'TITVVTC'
173400070326     C                   parm                    parccm
173500070326     C                   parm                    parmbr
173600070326     C                   parm      '2'           paropz
173700070326     C*
173800070326     C                   eval      wrkesito = '2'
173900070404     C*
174000070404     C                   seton                                        LR
174100070326     C*
174200070326     C                   ENDSR     '*CANCL'
174300070326     C***
174400080617
174500080617
174600080617
174700080617      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
174800080617     C     exeBon        BEGSR
174900080617     C*
175000080617     C* Imposto i parametri x il *pgm d "bonifica"
175100080617     C                   EVAL      ElencoChar = '1234567890'
175200080617     C                   EVAL      TipoElenco = *blanks
175300080617     C                   EVAL      CharSost   = *blanks
175400080617     C                   EVAL      UpperCase  = *blanks
175500080617     C                   EVAL      ChkNull    = '1'
175600080617     C                   EVAL      CharNull   = *blanks
175700080617     C*
175800080617     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VABRMA
175900080617     C                   EVAL      EsitoBon   = *blanks
176000080617     C*
176100080617     C                   CALL      'XCHKCHAR'
176200080617     C                   PARM                    TxtInOut
176300080617     C                   PARM                    ElencoChar
176400080617     C                   PARM                    TipoElenco
176500080617     C                   PARM                    CharSost
176600080617     C                   PARM                    UpperCase
176700080617     C                   PARM                    ChkNull
176800080617     C                   PARM                    CharNull
176900080617     C                   PARM                    EsitoBon
177000080617     C*
177100080617     C                   ENDSR
177200080626
177300080626
177400080626
177500080626     C*----------------------------------------------------*
177600080626     C*  FORZATURE SPECIFICHE X IL CLIENTE CORRENTE
177700080626     C*----------------------------------------------------*
177800080626     C     EXEFORZ       BEGSR
177900080626     C*
178000080626     C                   movel     '","'         wFrom             3
178100080626     C                   movel     ' ; '         wTo               3
178200080626     C*
178300080626     C                   eval      depvindta = vindta
178400080626     C*
178500080626     C* Forzatura x distinzione delimitatore campo e delimitare decimale
178600080626     C                   eval      vindta = '",'+%trim(vindta)
178700080627     C                   dow       %scan(wFrom:vindta) > *zeros
178800080627     C                   eval      vindta = %replace(wTo:vindta:
178900080627     C                                         %scan(wFrom:vindta))
179000080627     C                   enddo
179100080626     C*
179200080627     C                   eval      vindta = %trim(vindta)
179300080627     C                   DOW       %subst(vindta:1:1)=CharCSV                   * all'inizio
179400080626     C                   EVAL      vindta = %subst(vindta:2)
179500080627     C                   eval      vindta = %trim(vindta)
179600080626     C                   ENDDO
179700080627     C*
179800080627     C                   DOW       %subst(vindta:%len(%trim(vindta)):1)='"'     * alla fine
179900080627     C                   EVAL      vindta=%subst(vindta:1:%len(%trim(vindta))-1)
180000080627     C                   eval      vindta = %trim(vindta)
180100080627     C                   ENDDO
180200080626     C*
180300080626     C                   ENDSR
180400070326
180500070326
180600010330
180700010330
180800000613     C     *inzsr        BEGSR
180900990910     C*
181000990910     C     *entry        plist
181100990920     C                   parm                    tivlrds
181200990921     C                   parm      wrkesito      esito
181300000724     C                   parm                    prmlit
181400000710     C                   parm                    prmfir
181500010330     C*
181600010330     C* CALCOLA LA DATA CORRENTE
181700010330     C                   time                    wn14             14 0
181800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
181900010330     C                   z-add     wn8           g08dat
182000010330     C                   z-add     *zeros        g08inv
182100010330     C                   movel     '0'           g08err
182200010330     C                   call      'XSRDA8'
182300010330     C                   parm                    wlbda8
182400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
182500000613     C*
182600000613     C                   ENDSR
182700000613     C***
182800990908
182900080626     Otitv1h5p  E            riepilogo         2
183000990915     O                                              'Upload via Internet'
183100990915     O                                           +1 'Traduzione TIVIN00R -'
183200021025     O                                           +1 'FIVABWWR'
183300990915     O                                           +1 'Testate bolle'
183400990915     O          E            riepilogo   2
183500990915     O                       wrkdata
183600990915     O                       wrkora              +1
183700990915     O                       procname            +1
183800990915     O          E            riepilogo   2
183900990915     O                                              'Cliente..................:'
184000010605     O                       VABCCM        z     +1
184100990915     O          E            riepilogo   2
184200990920     O                                              'Riferimento Strategi.....:'
184300990920     O                       vlrhdl              +1
184400990915     O          E            riepilogo   2
184500990915     O                                              'Giusti...................:'
184600040119     O                       §CTROKVB      2   +  1
184700990915     O          E            riepilogo   2
184800990915     O                                              'Sbagliati e corretti.....:'
184900971022     O                       §CTRMO        2   +  1
185000990915     O          E            riepilogo   2
185100990915     O                                              'Sbagliati e scartati.....:'
185200971022     O                       §CTRNO        2   +  1
185300000613
185400080626     Otitv1h5ps E            testdett          2
185500000613     O                                              'Upload via Internet'
185600000613     O                                           +1 'Traduzione TIVIN00R -'
185700021025     O                                           +1 'FIVABWWR'
185800000613     O                                           +1 'Testate bolle'
185900000616     O          E            testdett    3
186000000613     O                                           +2 'N° rec'
186100000613     O                                           +3 'Anteprima contenuto'
186200000616     O          E            rigadett    2
186300000613     O                       rrnum               +2
186400000621     O                       recko               +3
186500000616     O          E            findett     2
186600000613     O                       wrkdata
186700000613     O                       wrkora              +1
186800000613     O                       procname            +1
186900000616     O          E            findett     2
187000000613     O                                              'Cliente..................:'
187100010605     O                       VABCCM        z     +1
187200000616     O          E            findett     2
187300000613     O                                              'Riferimento Strategi.....:'
187400000613     O                       vlrhdl              +1
187500000616     O          E            findett     2
187600000613     O                                              'Giusti...................:'
187700040119     O                       §CTROKVB      2   +  1
187800000616     O          E            findett     2
187900000613     O                                              'Sbagliati e corretti.....:'
188000000613     O                       §CTRMO        2   +  1
188100000616     O          E            findett     2
188200000613     O                                              'Sbagliati e scartati.....:'
188300000613     O                       §CTRNO        2   +  1
188400000613** CMD - COMANDI CL
188500080626OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
188600080626OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1H5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
188700080626DLTOVR FILE(TITV1H5P TITV1H5PS) LVL(*)
188800010606
188900000613
189000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
189100080626"FILL01","FILL02","W_VABRMN","FILL03","VABRSD","VABIND","VABCAD","VABLOD",
189200080711"VABPRD","VABNCL","VABPKB","VABNOT","FILL04","VATNOT_A","FILL05","VABCAS",
189300080711"VATNOT_E","FILL06"
