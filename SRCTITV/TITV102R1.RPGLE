000100081124      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVADWWR/FIVATWWR.
000200081124     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700070117     FFIVADwwr  O    E             DISK    usropn
000800081124     FFIVATwwr  O    E             DISK    usropn
000900040122     Ftitv92p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100040122     Ftitv92ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300000313     D*
001400000613     D*------------
001500000613     D* COMANDI
001600000613     D*------------
001700011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001800040113     D*------------
001900040113     D* SCHIEDE A PROGRAMMA
002000040113     D*------------
002100040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002200010330     D*----------------------------------------------------
002300010330     D* DICHIARAZIOINE VARIABILI DI WRK
002400010330     D*----------------------------------------------------
002500010330     D dscmz         e ds                  inz
002600010330     D psds           sds
002700010330     D  procname         *PROC
002800010330     D tivlrds       e ds                  extname(tivlr00f)
002900040128     D tisi95ds      e ds
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D wrkdata         s               d
003500010330     D wrkora          s               t
003600010330     D rrnum           s              6  0 INZ(*zeros)
003700010330     D recko           s            150    INZ(*blanks)
003800011113     D depcmd          s            150    INZ(*blanks)
003900040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004000040510     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600070117     D wVADCDP_1       s                   LIKE(VADCDP)
005700070117     D wVADNCL_1       s                   LIKE(VADNCL)
005800070117     D wVADCDP_2       s                   LIKE(VADCDP)
005900070117     D wVADNCL_2       s                   LIKE(VADNCL)
006000070117     D wVADCDP_3       s                   LIKE(VADCDP)
006100070117     D wVADNCL_3       s                   LIKE(VADNCL)
006200070117     D wVADCDP_4       s                   LIKE(VADCDP)
006300070117     D wVADNCL_4       s                   LIKE(VADNCL)
006400070117     D wVADCDP_5       s                   LIKE(VADCDP)
006500070117     D wVADNCL_5       s                   LIKE(VADNCL)
006600060331     D wVABNSP         s                   LIKE(VABNSP) INZ
006700081124     D wVATNOT_A       s                   LIKE(VATNOT)
006800081124     D wVATNOT_B       s                   LIKE(VATNOT)
006900081124     D wVATNOT_E       s                   LIKE(VATNOT)
007000111116     D wVATNOT_I       s                   LIKE(VATNOT)
007100111116     D wVATNOT_J       s                   LIKE(VATNOT)
007200030822     D*------------------
007300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007400030822     D*------------------
007500030822     D posDaDft        S              3  0 INZ(*zeros)
007600030822     D posADft         S              3  0 INZ(*zeros)
007700030822     D j               s              3  0 INZ(1)
007800030822     D wGiroDft        s              1  0 INZ(*zeros)
007900010330     D*------------------
008000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
008100010330     D*------------------
008200010330     D WLBDA8          DS                  INZ
008300010330     D  G08DAT                 1      8  0
008400010330     D  G08INV                 9     16  0
008500010330     D  G08ERR                17     17
008600010330     D  G08TGI                18     22  0
008700031201     D*------------------
008800031201     D* Costanti
008900031201     D*------------------
009000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200081124
009300081124     D*------------------
009400081124     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
009500081124     D*------------------
009600081124     D PiStr           S            100A   INZ
009700081124     D PiDecChr        S              1A   INZ
009800081124     D PiVal           S             63S30 INZ
009900081124     D PiInt           S               N   INZ
010000081124     D PiNum           S               N   INZ
010100081124
010200081124     D*------------------
010300081124     D* LINKING A DEFINIZIONI ESTERNE
010400081124     D*------------------
010500081124     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010600081124
010700010330
010800010330
010900010330
011000990915     C                   time                    wrkdata
011100990915     C                   time                    wrkora
011200000913     C                   reset                   rrnum
011300990921     C                   reset                   esito
011400990921     C                   reset                   wrkesito
011500010601     C*
011600010601     C                   exsr      opeini
011700010605     C                   exsr      rwvab
011800081006     C*
011900081006     C                   exsr      endela
012000010601     C*
012100010601     C                   seton                                        lr
012200010601
012300010601
012400010601
012500010601     C*--------------------------------------------------------
012600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012700010601     C*--------------------------------------------------------
012800010601     C     PREELA        BEGSR
012900010601     C*
013000010601     C* SE OCCORRE SPEDIRE IN FILIALE
013100010601     C                   if        invfil <> *zeros and
013200010601     C                             flgGiro = '0'
013300010601     C*
013400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013500010601     C                   eval      flgGiro = '1'
013600000724     C*
013700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013900010601     C     invfil        chain     azorg01l
014000000724     C                   if        %found
014100000724     C                   movel(p)  CMD(1)        depcmd
014200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
014300000724     C*
014400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014500011113     C                   Z-ADD     150           LENGH            15 5
014600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700000724     C                   PARM                    depcmd
014800000724     C                   PARM                    LENGH
014900000724     C*
015000000724     C                   endif
015100000616     C*
015200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015300000616     C                   movel(p)  CMD(2)        depcmd
015400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015500000616     C*
015600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015700011113     C                   Z-ADD     150           LENGH            15 5
015800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015900000616     C                   PARM                    depcmd
016000000616     C                   PARM                    LENGH
016100000613     C*
016200040122     C                   if        not %open(titv92ps)
016300040122     C                   open      titv92ps
016400030715     C                   except    testdett
016500000613     C                   endif
016600010601     C*
016700010601     C                   endif
016800010601     C*
016900010601     C                   ENDSR
017000010601     C***
017100010601
017200010601
017300010601
017400010601
017500010601     C*--------------------------------------------------------
017600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017700010601     C*--------------------------------------------------------
017800081006     C     ENDELA        BEGSR
017900990915     C*
018000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
018100081006     C*
018200081006     C                   if        %open(tivin00r)
018300081006     C                   close     tivin00r
018400081006     C                   endif
018500081006     C                   if        %open(fivabwwr)
018600081006     C                   close     fivabwwr
018700081006     C                   endif
018800081006     C                   if        %open(fivadwwr)
018900081006     C                   close     fivadwwr
019000081006     C                   endif
019100081124     C                   if        %open(fivatwwr)
019200081124     C                   close     fivatwwr
019300081124     C                   endif
019400040119     C*
019500040119     C* Effettuo la chiamata al CLLE preposto
019600081124     C                   call(e)   'TITVVDTC'
019700040510     C                   parm                    parccm
019800040119     C                   parm                    parmbr
019900040119     C                   parm      '2'           paropz
020000050201     C*
020100050201     C* Effettuo lancio TISI95 solo x chiusura
020200050201     C                   CLEAR                   TISI95DS
020300050201     C                   EVAL      I95TLA = 'C'
020400050201     C                   CALL      'TISI95R'
020500050201     C                   PARM                    TISI95DS
020600000613     C*
020700040122     C                   if        %open(titv92ps)
020800000616     C                   except    findett
020900040122     C                   close     titv92ps
021000000613     C                   endif
021100000616     C*
021200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
021300011113     C                   Z-ADD     150           LENGH            15 5
021400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
021500000616     C                   PARM                    CMD(3)
021600000616     C                   PARM                    LENGH
021700000616     C*
021800010601     C                   ENDSR
021900010601     C***
022000010601
022100010601
022200010601
022300000613
022400000613     C*--------------------------------------------------------
022500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
022600000613     C*--------------------------------------------------------
022700000621     C     STPR          BEGSR
022800000613     C*
022900040122     C                   if        not %open(titv92p)
023000040122     C   50              open      titv92p
023100990915     C                   endif
023200990915     C*
023300030715     C   50              except    riepilogo
023400990915     C*
023500040122     C                   if        %open(titv92p)
023600040122     C   50              close     titv92p
023700990914     C                   endif
023800000613     C*
023900000613     C                   ENDSR
024000000613     C***
024100000613
024200010601
024300010601
024400010330     C*--------------------------------------------------------
024500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
024600010330     C*--------------------------------------------------------
024700010605     C     RWVAB         BEGSR
024800010330     C*
024900010330     C                   if        not %open(tivin00r)
025000010330     C                   open      tivin00r
025100010330     C                   endif
025200021025     C                   if        not %open(fivabwwr)
025300021025     C                   open      fivabwwr
025400010330     C                   endif
025500040119     C*
025600070117     C* Eseguo operazioni di aggiunta nuovo membro in FIVADWWR
025700081124     C                   exsr      prevadt
025800040119     C*
025900040119     C                   if        chkcall = '0'
026000040119     C*
026100070117     C                   if        not %open(fivadwwr)
026200070117     C                   open      fivadwwr
026300040119     C                   endif
026400081124     C                   if        not %open(fivatwwr)
026500081124     C                   open      fivatwwr
026600081124     C                   endif
026700010330     C*
026800040119     C                   clear                   §CTROKVB          7 0
026900070117     C                   clear                   §CTROKVD          7 0
027000081124     C                   clear                   §CTROKVT          7 0
027100040119     C                   clear                   §CTRMO            7 0
027200040119     C                   clear                   §CTRNO            7 0
027300010330     C*
027400030822     C                   do        *HIVAL
027500010330     C*
027600030822     C                   read      tivin00r                               70
027700010618     C*
027800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027900010618     C                   if        vinflg = '1'
028000010618     C                   eval      flgOk = '1'
028100010618     C                   endif
028200040510     C*
028300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
028400040510     C                   z-add     *zeros        wLenVINDTA        4 0
028500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
028600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
028700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
028800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
028900040510     C                   z-add     1             wFlgVINDTA
029000040510     C                   leave
029100040510     C                   endif
029200040510     C                   enddo
029300010618     C*
029400040510     C                   if        vindta > *blanks AND
029500040510     C                             wFlgVINDTA = 1
029600010330     C                   add       1             rrnum
029700010330     C*
029800010601     C                   if        *in70 = *off and
029900010330     C                             (vinflg = *blanks
030000010330     C                              or vinflg = '0'
030100010330     C                              or vinflg = '2')
030200010330     C*
030300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
030400010711     C                   if        vinflg = *blanks or vinflg = '0'
030500010711     C                   clear                   vinmsg
030600010711     C                   endif
030700010601     C*
030800010330     C                   exsr      inzvar
030900121127     C*
031000121127     C* Aggiungo "x sicurezza" sempre un carattere separatore campo alla fine dei dati
031100121127     C                   eval      vindta = %trim(vindta)+CharCSV
031200121127     C*
031300010605     C                   exsr      impvab
031400010601     C*
031500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
031600010605     C                   eval      depfil = VABLNP
031700010601     C                   exsr      repfil
031800010601     C                   if        depfil = invfil
031900021025     C                   if        vlrpoi = 999
032000030822     C                   move(P)   invfil        VABFGS
032100021025     C                   else
032200030822     C                   move(P)   vlrpoi        VABFGS
032300021025     C                   endif
032400010601     C*
032500010601     C                   exsr      PREELA
032600010601     C*
032700010604     C* Ebbene...
032800010604     C*
032900040119     C  N31              add       1             §CTROKVB
033000040119     C   32              add       1             §CTRMO
033100040119     C   31              add       1             §CTRNO
033200030822     C                   if        wGiro = 2
033300070117     C* Scrivo sempre il VAD
033400070117     C  N31              exsr      wrivad
033500040510     C* Scrivo il VAB solo a rottura d spedizione
033600040119     C                   if        depspe =  *zeros OR
033700040119     C                             depspe <> vabNSP
033800040119     C  N31              exsr      wrivab
033900111116     C  N31              exsr      wrivat
034000040119     C  N31              eval      depspe = vabNSP
034100040119     C                   endif
034200040119     C                   endif
034300010604     C*
034400010604     C                   if        *in31 = *off and
034500010604     C                             *in32 = *off
034600010604     C                   eval      vinflg = '1'
034700010604     C                   else
034800010604     C                   eval      recko = vindta
034900010604     C                   except    rigadett
035000010604     C                   eval      vinflg = '2'
035100010604     C                   endif
035200010604     C                   endif
035300010601     C*
035400010604     C                   endif
035500010604     C*
035600010330     C                   else
035700010330     C                   eval      vinflg = '1'
035800010330     C                   endif
035900010601     C*
036000010601     C  N70              update    tivin000
036100010330     C*
036200030822     C  N70              enddo
036300040119     C*
036400040119     C                   endif                                                  (endif - chkcall)
036500010601     C*
036600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
036700010601     C                   if        cntNonEl = *zeros or
036800010601     C                             flgMulti = '0'
036900010330     C* Se non ci sono record con errori ...
037000010601     C                   if        §ctrno = 0 and
037100010604     C                             §ctrmo = 0 and
037200010601     C                             flgStato <> '2'
037300010330     C* ... restituisco esito OK.
037400010330     C                   eval      wrkesito = '0'
037500010330     C                   else
037600040119     C                   if        §ctrokvb > 0
037700010330     C                   eval      wrkesito = '1'
037800010330     C                   else
037900010615     C                   if        flgOk = '0'
038000010615     C                   eval      wrkesito = '2'
038100010615     C                   else
038200010615     C                   eval      wrkesito = '6'
038300010615     C                   endif
038400010330     C                   endif
038500010330     C                   endif
038600010601     C                   else
038700010601     C                   eval      wrkesito = '9'
038800010601     C                   endif
038900010601     C*
039000010601     C                   if        vlrpoi <> 999
039100010601     C                   eval      invfil = vlrpoi
039200010601     C                   endif
039300010330     C*
039400040119     C                   if        §ctrokvb > 0
039500010601     C                             and invfil > *zeros
039600010330     C                   exsr      invio
039700010330     C                   endif
039800010601     C*
039900111116     C***                if        flgGiro = '1'
040000111116     C***                exsr      endela
040100111116     C***                endif
040200010330     C*
040300010330     C                   ENDSR
040400010330     C***
040500040119
040600040119
040700040119
040800040119     C*----------------------------------------------------*
040900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
041000040119     C*----------------------------------------------------*
041100040510     C     WRIVAB        BEGSR
041200040119     C*
041300060331     C* Gestisco l'eventuale rottura x numero spedizione
041400060331     C                   if        VABNSP <> wVABNSP
041500081124     C                   if        wVATNOT_E <> *blanks
041600081124     C                   eval      VABCTM = '7Q'
041700081124     C                   endif
041800040119     C                   write     fivab000                                     => scarico il VAB
041900060331     C                   eval      wVABNSP = VABNSP
042000060331     C                   endif
042100040119     C*
042200040119     C                   ENDSR
042300040119
042400040119
042500040119
042600040119     C*----------------------------------------------------*
042700070117     C*  SCARICAMENTO BUFFER RECORDS VAD
042800040119     C*----------------------------------------------------*
042900070117     C     WRIVAD        BEGSR
043000060512     C*
043100060512     C* Inizializzo i campi chiave
043200070117     C                   eval      VADFGS = VABFGS
043300070117     C                   eval      VADCCM = VABCCM
043400070117     C                   eval      VADAAS = VABAAS
043500070117     C                   eval      VADLNP = VABLNP
043600070117     C                   eval      VADNRS = VABNRS
043700070117     C                   eval      VADNSP = VABNSP
043800060331     C*
043900070117     C* Gestisco estensioni VAD "specificatamente esplicitate"
044000070117     C                   if        wVADCDP_1 <> *blanks AND
044100070117     C                             wVADNCL_1  > *zeros
044200070117     C                   eval      VADCDP = wVADCDP_1
044300070117     C                   eval      VADNCL = wVADNCL_1
044400070117     C                   write     FIVAD000
044500070117     C                   add       1             §CTROKVD
044600060331     C                   endif
044700040119     C*
044800070117     C                   if        wVADCDP_2 <> *blanks AND
044900070117     C                             wVADNCL_2  > *zeros
045000070117     C                   eval      VADCDP = wVADCDP_2
045100070117     C                   eval      VADNCL = wVADNCL_2
045200070117     C                   write     FIVAD000
045300070117     C                   add       1             §CTROKVD
045400070117     C                   endif
045500070117     C*
045600070117     C                   if        wVADCDP_3 <> *blanks AND
045700070117     C                             wVADNCL_3  > *zeros
045800070117     C                   eval      VADCDP = wVADCDP_3
045900070117     C                   eval      VADNCL = wVADNCL_3
046000070117     C                   write     FIVAD000
046100070117     C                   add       1             §CTROKVD
046200070117     C                   endif
046300070117     C*
046400070117     C                   if        wVADCDP_4 <> *blanks AND
046500070117     C                             wVADNCL_4  > *zeros
046600070117     C                   eval      VADCDP = wVADCDP_4
046700070117     C                   eval      VADNCL = wVADNCL_4
046800070117     C                   write     FIVAD000
046900070117     C                   add       1             §CTROKVD
047000070117     C                   endif
047100070117     C*
047200070117     C                   if        wVADCDP_5 <> *blanks AND
047300070117     C                             wVADNCL_5  > *zeros
047400070117     C                   eval      VADCDP = wVADCDP_5
047500070117     C                   eval      VADNCL = wVADNCL_5
047600070117     C                   write     FIVAD000
047700070117     C                   add       1             §CTROKVD
047800070117     C                   endif
047900070117     C*
048000040119     C                   ENDSR
048100081124
048200081124
048300081124
048400081124     C*----------------------------------------------------*
048500081124     C*  SCARICAMENTO BUFFER RECORDS VAT
048600081124     C*----------------------------------------------------*
048700081124     C     WRIVAT        BEGSR
048800081124     C*
048900081124     C* Inizializzo i campi chiave
049000081124     C                   eval      VATFGS = VABFGS
049100081124     C                   eval      VATAAS = VABAAS
049200081124     C                   eval      VATLNP = VABLNP
049300081124     C                   eval      VATNRS = VABNRS
049400081124     C                   eval      VATNSP = VABNSP
049500081124     C                   eval      VATCCM = VABCCM
049600081124     C*
049700081124     C* Gestisco estensioni VAT "specificatamente esplicitate"
049800081124     C                   if        wVATNOT_A <> *blanks
049900081124     C                   eval      VATNOT = wVATNOT_A
050000081124     C                   eval      VATTRC = 'A'
050100081124     C                   write     FIVAT000
050200081124     C                   add       1             §CTROKVT
050300081124     C                   endif
050400081124     C*
050500081124     C                   if        wVATNOT_B <> *blanks
050600081124     C                   eval      VATNOT = wVATNOT_B
050700081124     C                   eval      VATTRC = 'B'
050800081124     C                   write     FIVAT000
050900081124     C                   add       1             §CTROKVT
051000081124     C                   endif
051100081124     C*
051200081124     C                   if        wVATNOT_E <> *blanks
051300081124     C                   eval      VATNOT = wVATNOT_E
051400081124     C                   eval      VATTRC = 'E'
051500081124     C                   write     FIVAT000
051600081124     C                   add       1             §CTROKVT
051700081124     C                   endif
051800111116     C*
051900111116     C                   if        wVATNOT_I <> *blanks
052000111116     C                   eval      VATNOT = wVATNOT_I
052100111116     C                   eval      VATTRC = 'I'
052200111116     C                   write     FIVAT000
052300111116     C                   add       1             §CTROKVT
052400111116     C                   endif
052500111116     C*
052600111116     C                   if        wVATNOT_J <> *blanks
052700111116     C                   eval      VATNOT = wVATNOT_J
052800111116     C                   eval      VATTRC = 'J'
052900111116     C                   write     FIVAT000
053000111116     C                   add       1             §CTROKVT
053100111116     C                   endif
053200081124     C*
053300081124     C                   ENDSR
053400040119
053500040119
053600040119
053700040119     C*----------------------------------------------------*
053800081124     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVADWWR/FIVATWWR
053900040119     C*----------------------------------------------------*
054000081124     C     PREVADT       BEGSR
054100040119     C*
054200081124     C* Compongo il nome del membro da dare al FIVADWWR/FIVATWWR
054300040119     C                   eval      parmbr = vlrhdl
054400040119     C                   movel     'M'           parmbr
054500040510     C                   eval      parccm = vlrksc
054600040119     C                   eval      paropz = '1'
054700040119     C* Effettuo la chiamata al CLLE preposto
054800081124     C                   call(e)   'TITVVDTC'
054900040119     C                   parm                    parccm
055000040119     C                   parm                    parmbr
055100111116     C                   parm      '1'           paropz
055200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055300040119     C                   if        %error
055400040119     C                   movel     '1'           chkcall
055500040119     C                   else
055600040119     C                   movel     '0'           chkcall
055700040119     C                   endif
055800040119     C*
055900040119     C                   ENDSR
056000010601
056100010601
056200010601
056300010330     C*----------------------------------------------------*
056400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
056500010330     C*----------------------------------------------------*
056600040119     C     INZVAR        BEGSR
056700010330     C*
056800030822     C* Inizializzo variabili di wrk
056900020204     C                   Z-ADD     *zeros        Num5_0            5 0
057000070117     C                   MOVEL     *blanks       wVADCDP_1
057100070117     C                   MOVEL     *blanks       wVADCDP_2
057200070117     C                   MOVEL     *blanks       wVADCDP_3
057300070117     C                   MOVEL     *blanks       wVADCDP_4
057400070117     C                   MOVEL     *blanks       wVADCDP_5
057500070117     C                   Z-ADD     *zeros        wVADNCL_1
057600070117     C                   Z-ADD     *zeros        wVADNCL_2
057700070117     C                   Z-ADD     *zeros        wVADNCL_3
057800070117     C                   Z-ADD     *zeros        wVADNCL_4
057900070117     C                   Z-ADD     *zeros        wVADNCL_5
058000081124     C*
058100081124     C                   MOVEL     *blanks       wVATNOT_A
058200081124     C                   MOVEL     *blanks       wVATNOT_B
058300081124     C                   MOVEL     *blanks       wVATNOT_E
058400111116     C                   MOVEL     *blanks       wVATNOT_I
058500111116     C                   MOVEL     *blanks       wVATNOT_J
058600030822     C*
058700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
058800030822     C                   CLEAR                   FIVAB000
058900070117     C                   CLEAR                   FIVAD000
059000030822     C                   CLEAR                   SkSplitCSV
059100030822     C                   EVAL      i = 1
059200030822     C                   EVAL      posDa = *zeros
059300030822     C                   EVAL      posA  = *zeros
059400030822     C*
059500030822     C* Reimposto i valori di default
059600030822     C                   EXSR      DEFCAM
059700030822     C*
059800010330     C                   ENDSR
059900010330     C*----------------------------------------------------*
060000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
060100010330     C*----------------------------------------------------*
060200010330     C     DEFCAM        BEGSR
060300030822     C*
060400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
060500030715     C* e delimitatore testo.
060600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
060700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
060800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
060900030822     C*
061000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
061100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
061200030715     C                   SETON                                        50
061300030715     C                   ELSE
061400030715     C                   SETOFF                                       50
061500030715     C                   ENDIF
061600030822     C*
061700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
061800030822     C                   EVAL      posDaDft = 1
061900030822     C                   EVAL      posADft  = 0
062000030822     C                   EVAL      wGiroDft = 0
062100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
062200030822     C                             posDaDft > 0
062300030822     C*
062400030822     C* Gestisco il 1° giro
062500030822     C                   IF        wGiroDft = 0
062600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
062700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
062800030822     C* Incremento il contatore dei "giri"
062900030822     C                   EVAL      wGiroDft = 1
063000030822     C                   ELSE
063100030822     C                   EVAL      posDaDft = posADft
063200030822     C                   ENDIF
063300030822     C* Eseguo lo scan x trovare la fine del campo corrente
063400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
063500030822     C*
063600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
063700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
063800030822     C                   IF        posDaDft > 0 AND
063900030822     C                             posADft  > 0
064000030822     C* NCL
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'NCL'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABNCL
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* CCM
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'CCM'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABCCM
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* LNP
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'LNP'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABLNP
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* NRS
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'NRS'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiInt=*on
068900030822     C                   Z-ADD     PiVal         VABNRS
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* CTR
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'CTR'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiInt=*on
070200030822     C                   Z-ADD     PiVal         VABCTR
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* PKB
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'PKB'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiNum=*on
071500030822     C                   Z-ADD     PiVal         VABPKB
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* VLB
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VLB'
072300030822     C                   EVAL      PiStr=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   EXSR      CHKNUM
072700030822     C                   IF        PiNum=*on
072800030822     C                   Z-ADD     PiVal         VABVLB
072900030822     C                   ENDIF
073000030822     C                   ENDIF
073100030822     C* QFT
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'QFT'
073600030822     C                   EVAL      PiStr=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   EXSR      CHKNUM
074000030822     C                   IF        PiNum=*on
074100030822     C                   Z-ADD     PiVal         VABQFT
074200030822     C                   ENDIF
074300030822     C                   ENDIF
074400030822     C* CBO
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'CBO'
074900030822     C                   EVAL      VABCBO=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300030822     C* TSP
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'TSP'
075800030822     C                   EVAL      VABTSP=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200030822     C* VAS
076300030822     C                   IF        %subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):1:3)
076600030822     C                             = 'VAS'
076700030822     C                   EVAL      VABVAS=%trim(%subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000030822     C                   ENDIF
077100030822     C* VCA
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'VCA'
077600030822     C                   EVAL      VABVCA=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000030822     C* GCA
078100030822     C                   IF        %subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):1:3)
078400030822     C                             = 'GCA'
078500030822     C                   EVAL      VABGCA=%trim(%subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800030822     C                   ENDIF
078900030822     C* CTM
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'CTM'
079400030822     C                   EVAL      VABCTM=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* FFD
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'FFD'
080300030822     C                   EVAL      VABFFD=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700030822     C* VAD
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100030822     C                             = 'VAD'
081200030822     C                   EVAL      VABVAD=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600030822     C* GMA
081700030822     C                   IF        %subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):1:3)
082000030822     C                             = 'GMA'
082100030822     C                   EVAL      VABGMA=%trim(%subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400030822     C                   ENDIF
082500030822     C* GGA
082600030822     C                   IF        %subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):1:3)
082900030822     C                             = 'GGA'
083000030822     C                   EVAL      VABGGA=%trim(%subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083300030822     C                   ENDIF
083400030822     C* GVA
083500030822     C                   IF        %subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):1:3)
083800030822     C                             = 'GVA'
083900030822     C                   EVAL      VABGVA=%trim(%subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084200030822     C                   ENDIF
084300030822     C* TC1
084400030822     C                   IF        %subst(
084500030822     C                             %subst(vlrppt:posDaDft+1:
084600030822     C                             posADft-posDaDft-1):1:3)
084700030822     C                             = 'TC1'
084800030822     C                   EVAL      VABTC1=%trim(%subst(
084900030822     C                             %subst(vlrppt:posDaDft+1:
085000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085100030822     C                   ENDIF
085200030822     C* TC2
085300030822     C                   IF        %subst(
085400030822     C                             %subst(vlrppt:posDaDft+1:
085500030822     C                             posADft-posDaDft-1):1:3)
085600030822     C                             = 'TC2'
085700030822     C                   EVAL      VABTC2=%trim(%subst(
085800030822     C                             %subst(vlrppt:posDaDft+1:
085900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086000030822     C                   ENDIF
086100030822     C* ...
086200030822     C                   ENDIF
086300030822     C                   ENDDO
086400020204     C*
086500010330     C                   ENDSR
086600010607     C*----------------------------------------------------*
086700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086800010607     C*----------------------------------------------------*
086900010607     C     IMPVAB        BEGSR
087000010607     C*
087100010607     C                   SETOFF                                       3132
087200030515     C*
087300010607     C                   MOVEL     datcor        VABAAS
087400010607     C                   MOVE      datcor        VABMGS
087500010607     C*
087600030715     C**********
087700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087800030715     C**********
087900030715     C                   IF        wGiro = *zeros
088000040113     C*
088100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
088200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
088300040510     C                             %trim(intcol(3))<>*blanks
088400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088500040113     C                                      %trim(intcol(3))
088600040510     C                   ENDIF
088700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088800031201     C     minu:maiu     XLATE     vindta        vindta
088900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089000030715     C                   DOW       posDa <= %len(%trim(vindta))
089100030715     C*
089200030715     C* Gestisco il 1° campo
089300030715     C                   IF        i = 1
089400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089500030715     C                   EVAL      posDa = 1
089600030715     C* Eseguo lo scan x trovare la fine del primo campo
089700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089800030715     C* A questo punto "estraggo" il campo corrente
089900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090000030715     C                                                    (posA-posDa))
090100030715     C* X i campi successivi al 1°
090200030715     C                   ELSE
090300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090400030715     C                   EVAL      posDa = posA + 1
090500030715     C* Eseguo lo scan x trovare la fine del campo corrente
090600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090700030715     C* Gestisco l'ultimo campo
090800030715     C                   IF        posA = *zeros
090900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091000030715     C* A questo punto "estraggo" il campo corrente
091100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091200030715     C                                                    (posA-posDa))
091300030715     C                   LEAVE
091400030715     C                   ELSE
091500030715     C* A questo punto "estraggo" il campo corrente
091600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091700030715     C                                                    (posA-posDa))
091800030715     C                   ENDIF
091900030715     C                   ENDIF
092000030715     C* Incremento il contatore d campo
092100030715     C                   EVAL      i = i +1
092200030715     C                   ENDDO
092300030715     C*
092400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092500030715     C* delimitazione testo
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitFLD)
092800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093000030715     C                   EVAL      i = i + 1
093100030715     C                   ENDDO
093200030715     C                   EVAL      wGiro = 1
093300030715     C                   ELSE
093400030715     C                   EVAL      wGiro = 2
093500031204     C**********
093600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093700031204     C* d separatore campo
093800031204     C**********
093900031204     C                   EVAL      vindta = %trim(vindta)
094000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094100031204     C                   EVAL      vindta = %subst(vindta:2)
094200031204     C                   ENDDO
094300031204     C*
094400031204     C                   Z-ADD     *zeros        lunghInput        4 0
094500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094900031204     C                   ENDDO
095000030715     C**********
095100030715     C* Effettuo lo split del campo dati d input
095200030715     C**********
095300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095400030715     C                   DOW       posDa <= %len(%trim(vindta))
095500030715     C*
095600030715     C* Gestisco il 1° campo
095700030715     C                   IF        i = 1
095800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095900030715     C                   EVAL      posDa = 1
096000030715     C* Eseguo lo scan x trovare la fine del primo campo
096100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096200030715     C* A questo punto "estraggo" il campo corrente
096300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096400030715     C                                                    (posA-posDa))
096500030715     C* X i campi successivi al 1°
096600030715     C                   ELSE
096700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096800030715     C                   EVAL      posDa = posA + 1
096900031204     C* Verifico che nn vi sia il campo nn valorizzato
097000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097100031204     C* Se campo nn valorizzato skippo al prossimo
097200031204     C                   EVAL      SkSplitCSV(i) = *blanks
097300031204     C                   EVAL      posA  = posA + 1
097400031204     C                   ELSE
097500030715     C* Eseguo lo scan x trovare la fine del campo corrente
097600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097700030715     C* Gestisco l'ultimo campo
097800030715     C                   IF        posA = *zeros
097900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098000030715     C* A questo punto "estraggo" il campo corrente
098100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098200030715     C                                                    (posA-posDa))
098300030715     C                   LEAVE
098400030715     C                   ELSE
098500030715     C* A questo punto "estraggo" il campo corrente
098600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098700030715     C                                                    (posA-posDa))
098800030715     C                   ENDIF
098900031204     C                   ENDIF
099000030715     C                   ENDIF
099100030715     C* Incremento il contatore d campo
099200030715     C                   EVAL      i = i +1
099300030715     C                   ENDDO
099400030715     C*
099500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099600030715     C* delimitazione testo
099700030715     C                   EVAL      i = 1
099800030715     C                   DOW       i < %elem(SkSplitCSV)
099900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100100030715     C                   EVAL      i = i + 1
100200030715     C                   ENDDO
100300030715     C*
100400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100500030715     C                   EVAL      i = 1
100600030715     C                   DOW       i < %elem(SkSplitFLD)
100700030715     C*
100800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101000030715     C                             SkSplitCSV(i) <> *blanks
101100030721     C*** ATB
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101300030721     C                   EVAL      VABATB = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030715     C*** RSD
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102000030715     C* ==
102100030715     C                   ENDIF
102200030721     C*** RD2
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030715     C*** IND
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102800030715     C                   EVAL      VABIND = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030715     C*** LOD
103100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103300030715     C                   ENDIF
103400030715     C*** PRD
103500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103700030715     C                   ENDIF
103800030721     C*** NZD
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030715     C*** RMA
104300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104500030715     C                   ENDIF
104600030715     C*** NOT
104700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104900030715     C                   ENDIF
105000030715     C*** NT2
105100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105300030715     C                   ENDIF
105400030715     C*** VCA
105500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105700030715     C                   ENDIF
105800030715     C*** VAS
105900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106100030715     C                   ENDIF
106200030715     C*** FFD
106300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106500030715     C                   ENDIF
106600030721     C*** GC1
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** GC2
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** TSP
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107700030721     C                   ENDIF
107800040510     C*** CBO
107900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108100040510     C                   ENDIF
108200030721     C*** NAS
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** TIC
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** GCA
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** XCO
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** CTM
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** TCR
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** CTS
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** FTM
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** VAD
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** GMA
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*** GGA
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112500030721     C                   ENDIF
112600030721     C*** GVA
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112900030721     C                   ENDIF
113000030721     C*** TC1
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113300030721     C                   ENDIF
113400030721     C*** TC2
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113700030721     C                   ENDIF
113800030721     C*** SCL
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114100030721     C                   ENDIF
114200030721     C*** RMO
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114500030721     C                   ENDIF
114600030721     C*** NMO
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114900030721     C                   ENDIF
115000030721     C*
115100030721     C* Reperisco quindi i campi numerici...
115200040510     C*** NSP
115300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115400040510     C                   EVAL      PiStr=SkSplitCSV(i)
115500040510     C                   EXSR      CHKNUM
115600040510     C                   IF        PiInt=*on
115700040510     C                   Z-ADD     PiVal         VABNSP
115800040510     C                   ELSE
115900040510     C                   SETON                                        31
116000040510     C                   EVAL      VABNSP = *zeros
116100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116200040510     C                             + ' ' + 'VABNSP'
116300040510     C                   ENDIF
116400040510     C                   ENDIF
116500030721     C*** CAD
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiInt=*on
117000030721     C                   Z-ADD     PiVal         Num5_0
117100030721     C                   MOVEL(p)  Num5_0        VABCAD
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABCAD = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABCAD'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** NCL
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on
118400030721     C                   Z-ADD     PiVal         VABNCL
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABNCL = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900040122     C                             + ' ' + 'VABNCL'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** PKB
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiNum=*on
119700030721     C                   Z-ADD     PiVal         VABPKB
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      VABPKB = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABPKB'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** CAS
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiNum=*on
121000030721     C                   Z-ADD     PiVal         VABCAS
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      VABCAS = *zeros
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABCAS'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** CCM
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on      AND
122300030721     C                             PiVal<=9999999 AND
122400030721     C                             PiVal>*zeros
122500030721     C                   Z-ADD     PiVal         VABCCM
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      VABCCM = *zeros
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000040510     C                             + ' ' + 'VABCCM'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** LNP
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on    AND
123800030721     C                             PiVal<=999   AND
123900030721     C                             PiVal>*zeros
124000030721     C                   Z-ADD     PiVal         VABLNP
124100030721     C                   ELSE
124200040715     C                   SETON                                        32
124300030721     C                   EVAL      VABLNP = *zeros
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500040510     C                             + ' ' + 'VABLNP'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** NRS
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on AND
125300030721     C                             PiVal<=99
125400030721     C                   Z-ADD     PiVal         VABNRS
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABNRS'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** LNA
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on    AND
126600030721     C                             PiVal<=999   AND
126700030721     C                             PiVal>*zeros
126800030721     C                   Z-ADD     PiVal         VABLNA
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      VABLNA = *zeros
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABLNA'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** CTR
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on  AND
128100030721     C                             PiVal<=999
128200030721     C                   Z-ADD     PiVal         VABCTR
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABCTR'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** IAS
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiNum=*on
129400030721     C                   Z-ADD     PiVal         VABIAS
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABIAS'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** VLB
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiNum=*on
130600030721     C                   Z-ADD     PiVal         VABVLB
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABVLB'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** QFT
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiNum=*on
131800030721     C                   Z-ADD     PiVal         VABQFT
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABQFT'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** NCD
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on      AND
133000030721     C                             PiVal<=9999999
133100030721     C                   Z-ADD     PiVal         VABNCD
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABNCD'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** NCA
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on      AND
134300030721     C                             PiVal<=9999999
134400030721     C                   Z-ADD     PiVal         VABNCA
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABNCA'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** ZNC
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on AND
135600030721     C                             PiVal<=99
135700030721     C                   Z-ADD     PiVal         VABZNC
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABZNC'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** DCR
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on       AND
136900030721     C                             PiVal<=99999999
137000030721     C                   Z-ADD     PiVal         VABDCR
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABDCR'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700030721     C*** HCR
137800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiInt=*on   AND
138200030721     C                             PiVal<=9999
138300030721     C                   Z-ADD     PiVal         VABHCR
138400030721     C                   ELSE
138500030721     C                   SETON                                        32
138600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138700030721     C                             + ' ' + 'VABHCR'
138800030721     C                   ENDIF
138900030721     C                   ENDIF
139000030721     C*** VMD
139100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139200030721     C                   EVAL      PiStr=SkSplitCSV(i)
139300030721     C                   EXSR      CHKNUM
139400030721     C                   IF        PiNum=*on
139500030721     C                   Z-ADD     PiVal         VABVMD
139600030721     C                   ELSE
139700030721     C                   SETON                                        32
139800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139900030721     C                             + ' ' + 'VABVMD'
140000030721     C                   ENDIF
140100030721     C                   ENDIF
140200030721     C*** ANT
140300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140400030721     C                   EVAL      PiStr=SkSplitCSV(i)
140500030721     C                   EXSR      CHKNUM
140600030721     C                   IF        PiInt=*on        AND
140700030721     C                             PiVal<=999999999
140800030721     C                   Z-ADD     PiVal         VABANT
140900030721     C                   ELSE
141000030721     C                   SETON                                        32
141100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141200030721     C                             + ' ' + 'VABANT'
141300030721     C                   ENDIF
141400030721     C                   ENDIF
141500030721     C*** CMO
141600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141700030721     C                   EVAL      PiStr=SkSplitCSV(i)
141800030721     C                   EXSR      CHKNUM
141900030721     C                   IF        PiInt=*on
142000030721     C                   Z-ADD     PiVal         Num5_0
142100030721     C                   MOVEL(p)  Num5_0        VABCMO
142200030721     C                   ELSE
142300030721     C                   SETON                                        32
142400030721     C                   EVAL      VABCMO = *zeros
142500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142600030721     C                             + ' ' + 'VABCMO'
142700030721     C                   ENDIF
142800030721     C                   ENDIF
142900040510     C*
143000070117     C* Reperisco i campi del FIVAD
143100070117     C*** VADCDP_1
143200070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_1'
143300070117     C                   EVAL      wVADCDP_1 = %trim(SkSplitCSV(i))
143400060331     C                   ENDIF
143500070117     C*** VADCDP_2
143600070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_2'
143700070117     C                   EVAL      wVADCDP_2 = %trim(SkSplitCSV(i))
143800070117     C                   ENDIF
143900070117     C*** VADCDP_3
144000070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_3'
144100070117     C                   EVAL      wVADCDP_3 = %trim(SkSplitCSV(i))
144200070117     C                   ENDIF
144300070117     C*** VADCDP_4
144400070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_4'
144500070117     C                   EVAL      wVADCDP_4 = %trim(SkSplitCSV(i))
144600070117     C                   ENDIF
144700070117     C*** VADCDP_5
144800070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_5'
144900070117     C                   EVAL      wVADCDP_5 = %trim(SkSplitCSV(i))
145000070117     C                   ENDIF
145100070117     C*** VADNCL_1
145200070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_1'
145300070117     C                   EVAL      PiStr=SkSplitCSV(i)
145400070117     C                   EXSR      CHKNUM
145500070117     C                   IF        PiInt=*on
145600070117     C                   Z-ADD     PiVal         wVADNCL_1
145700070117     C                   ELSE
145800070117     C                   SETON                                        32
145900070117     C                   EVAL      wVADNCL_1 = *zeros
146000070117     C                   EVAL      vinmsg = %trimr(vinmsg)
146100070117     C                             + ' ' + 'VADNCL_1'
146200070117     C                   ENDIF
146300070117     C                   ENDIF
146400070117     C*** VADNCL_2
146500070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_2'
146600070117     C                   EVAL      PiStr=SkSplitCSV(i)
146700070117     C                   EXSR      CHKNUM
146800070117     C                   IF        PiInt=*on
146900070117     C                   Z-ADD     PiVal         wVADNCL_2
147000070117     C                   ELSE
147100070117     C                   SETON                                        32
147200070117     C                   EVAL      wVADNCL_2 = *zeros
147300070117     C                   EVAL      vinmsg = %trimr(vinmsg)
147400070117     C                             + ' ' + 'VADNCL_2'
147500070117     C                   ENDIF
147600070117     C                   ENDIF
147700070117     C*** VADNCL_3
147800070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_3'
147900070117     C                   EVAL      PiStr=SkSplitCSV(i)
148000070117     C                   EXSR      CHKNUM
148100070117     C                   IF        PiInt=*on
148200070117     C                   Z-ADD     PiVal         wVADNCL_3
148300070117     C                   ELSE
148400070117     C                   SETON                                        32
148500070117     C                   EVAL      wVADNCL_3 = *zeros
148600070117     C                   EVAL      vinmsg = %trimr(vinmsg)
148700070117     C                             + ' ' + 'VADNCL_3'
148800070117     C                   ENDIF
148900070117     C                   ENDIF
149000070117     C*** VADNCL_4
149100070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_4'
149200070117     C                   EVAL      PiStr=SkSplitCSV(i)
149300070117     C                   EXSR      CHKNUM
149400070117     C                   IF        PiInt=*on
149500070117     C                   Z-ADD     PiVal         wVADNCL_4
149600070117     C                   ELSE
149700070117     C                   SETON                                        32
149800070117     C                   EVAL      wVADNCL_4 = *zeros
149900070117     C                   EVAL      vinmsg = %trimr(vinmsg)
150000070117     C                             + ' ' + 'VADNCL_4'
150100070117     C                   ENDIF
150200070117     C                   ENDIF
150300070117     C*** VADNCL_5
150400070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_5'
150500070117     C                   EVAL      PiStr=SkSplitCSV(i)
150600070117     C                   EXSR      CHKNUM
150700070117     C                   IF        PiInt=*on
150800070117     C                   Z-ADD     PiVal         wVADNCL_5
150900070117     C                   ELSE
151000070117     C                   SETON                                        32
151100070117     C                   EVAL      wVADNCL_5 = *zeros
151200070117     C                   EVAL      vinmsg = %trimr(vinmsg)
151300070117     C                             + ' ' + 'VADNCL_5'
151400070117     C                   ENDIF
151500070117     C                   ENDIF
151600081124     C*
151700081124     C* Reperisco i campi del FIVAT
151800081124     C*** VATNOT_A
151900081124     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
152000081124     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
152100081124     C                   ENDIF
152200081124     C*** VATNOT_B
152300081124     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
152400081124     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
152500081124     C                   ENDIF
152600081124     C*** VATNOT_E
152700081124     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
152800081124     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
152900081124     C                   ENDIF
153000111116     C*** VATNOT_I
153100111116     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
153200111116     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
153300111116     C                   ENDIF
153400111116     C*** VATNOT_J
153500111116     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
153600111116     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
153700111116     C                   ENDIF
153800040510     C*
153900030715     C                   ENDIF
154000030715     C                   EVAL      i = i + 1
154100030715     C                   ENDDO
154200040510     C*
154300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154400040510     C                   IF        VABNSP = *zeros
154500040510     C                   SETON                                        31
154600040510     C                   ENDIF
154700040715     C*
154800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154900040715     C                   IF        VABLNP = *zeros
155000040715     C                   SETON                                        31
155100040715     C                   ENDIF
155200040510     C*
155300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155400040510     C                   IF        VABRMN = *zeros
155500040510     C                   Z-ADD     VABNSP        VABRMN
155600040510     C                   ENDIF
155700040510     C*
155800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155900040510     C                   IF        VABRMA = *blanks
156000040714     C                   MOVEL     VABRMN        VABRMA
156100040510     C                   ENDIF
156200040128     C*
156300040510     C* Se provincia nn valorizzata la reperisco
156400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156500040510     C                   IF        VABPRD = *blanks
156600040128     C                   CLEAR                   TISI95DS
156700040128     C                   EVAL      I95TCN = '3'
156800040128     C                   Z-ADD     datcor        I95DAT
156900040128     C                   EVAL      I95NAR = VABNZD
157000040128     C                   EVAL      I95CAP = VABCAD
157100040128     C                   EVAL      I95LOC = VABLOD
157200040128     C                   CALL      'TISI95R'
157300040128     C                   PARM                    TISI95DS
157400040128     C                   EVAL      VABPRD = O95PRV
157500040510     C                   ENDIF
157600040113     C*
157700040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
157800040113     C                   IF        VABCAS > *zeros
157900040119     C                   EVAL      VABVCA = 'EUR'
158000040119     C                   IF        VABCBO = '1'
158100040113     C                   EVAL      VABCBO = '4'
158200040119     C                   ENDIF
158300040119     C                   IF        VABCBO = '2'
158400040119     C                   EVAL      VABCBO = '6'
158500040119     C                   ENDIF
158600040113     C                   ENDIF
158700020204     C*
158800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158900020204     C                   EXSR      CHKIMPDIV
159000030715     C                   ENDIF
159100020204     C*
159200010607     C                   ENDSR
159300010607     C*----------------------------------------------------*
159400010601
159500020204
159600020204     C*----------------------------------------------------*
159700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159800020204     C*----------------------------------------------------*
159900020204     C     CHKIMPDIV     BEGSR
160000020204     C*
160100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
160200020204     C                   Z-ADD     *zeros        wrkDec            9 9
160300020204     C*
160400020204     C* Come prima cosa effettuo considerazioni sulla divisa
160500020204     C                   IF        vabIAS > *zeros
160600020204     C                   IF        vabVAS <> 'EUR'
160700020204     C                   EVAL      vabVAS =  'ITL'
160800020204     C                   ENDIF
160900020204     C                   ENDIF
161000020204     C*
161100020204     C                   IF        vabCAS > *zeros
161200020204     C                   IF        vabVCA <> 'EUR'
161300020204     C                   EVAL      vabVCA =  'ITL'
161400020204     C                   ENDIF
161500020204     C                   ENDIF
161600020204     C*
161700020204     C                   IF        vabVMD > *zeros
161800020204     C                   IF        vabVAD <> 'EUR'
161900020204     C                   EVAL      vabVAD =  'ITL'
162000020204     C                   ENDIF
162100020204     C                   ENDIF
162200020204     C*
162300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162400020204     C                   Z-ADD     vabIAS        wrkDec
162500020204     C                   IF        wrkDec > *zeros
162600020204     C                   IF        vabVAS = 'ITL'
162700020204     C                   EVAL      vabIAS = *zeros
162800020204     C                   ENDIF
162900020204     C                   ENDIF
163000020204     C*
163100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
163200020204     C                   Z-ADD     vabCAS        wrkDec
163300020204     C                   IF        wrkDec > *zeros
163400020204     C                   IF        vabVCA = 'ITL'
163500020204     C                   EVAL      vabCAS = *zeros
163600020204     C                   ENDIF
163700020204     C                   ENDIF
163800020204     C*
163900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
164000020204     C                   Z-ADD     vabVMD        wrkDec
164100020204     C                   IF        wrkDec > *zeros
164200020204     C                   IF        vabVAD = 'ITL'
164300020204     C                   EVAL      vabVMD = *zeros
164400020204     C                   ENDIF
164500020204     C                   ENDIF
164600020204     C*
164700020204     C                   ENDSR
164800020204     C***
164900020204
165000010330
165100010330
165200010330     C*----------------------------------------------------*
165300010330     C*  CONTROLLO NUMERICITA' CAMPI
165400010330     C*----------------------------------------------------*
165500081124     C     CHKNUM        BEGSR
165600081124     C*
165700081124     C                   IF        PiDecChr = *blanks
165800081124     C                   EVAL      PiDecChr = CharNUM
165900081124     C                   ENDIF
166000081124     C*
166100081124     C                   callp     UBISNUM_Check(PiStr
166200081124     C                                          :PiDecChr
166300081124     C                                          :PiVal
166400081124     C                                          :PiNum
166500081124     C                                          :PiInt)
166600010330     C*
166700010330     C                   ENDSR
166800010330     C***
166900010330
167000010601
167100010601
167200010601
167300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
167400010601     C     repfil        BEGSR
167500010601     C*
167600010601     C                   if        invfil = *zeros and
167700010601     C                             depfil > *zeros and
167800010629     C                             (vinflg = *blanks or
167900010629     C                              vinflg = *zeros)
168000010601     C
168100010601     C                   eval      invfil = depfil
168200010601     C                   endif
168300010601     C*
168400010601     C                   if        depfil <> invfil and
168500010601     C                             invfil > *zeros
168600010601     C                   eval      flgMulti = '1'
168700010601     C                   if        vinflg = *blanks
168800010601     C                   add       1             cntNonEl
168900010601     C                   endif
169000010601     C                   endif
169100010601     C*
169200010601     C                   if        vinflg = '2'
169300010601     C                   eval      flgStato = '2'
169400010601     C                   endif
169500010601     C*
169600010601     C                   ENDSR
169700010601     C***
169800010601
169900010601
170000010601
170100010330
170200040119      /TITLE Invio dei dati al punto operativo.
170300040119     C     invio         BEGSR
170400081124     C*
170500081124     C* 1° invio FIVAT
170600081124     C                   reset                   dscmz
170700081124     C                   move      vlrpoi        cmzdst
170800081124     C                   eval      cmzfld = 'FIVATWWR'
170900081124     C                   eval      cmzmbd = vlrhdl
171000081124     C                   eval      %subst(cmzmbd:1:1) = 'M'
171100081124     C***                if        prmfir = *blanks
171200081124     C                   eval      cmzfla = 'FIVAT00F'
171300081124     C                   eval      cmzmba = 'FIVAT00F'
171400081124     C***                else
171500081124     C***                eval      cmzfla = prmfir
171600081124     C***                eval      cmzmba = prmfir
171700081124     C***                endif
171800081124     C                   eval      cmznrr = *zeros
171900081124     C                   move      §ctrokvt      cmznrr
172000081124     C                   eval      cmzlba = vlrfl1
172100081124     C                   call(e)   'TIS711C'
172200081124     C                   parm                    dscmz
172300081124     C                   parm      *blanks       esito
172400081124     C                   if        %error
172500081124     C                             or cmzerr = '1'
172600081124     C                             or esito  = '1'
172700081124     C                   eval      wrkesito = '3'
172800081124     C                   else
172900040119     C*
173000081124     C* 2° invio FIVAD
173100040119     C                   reset                   dscmz
173200040119     C                   move      vlrpoi        cmzdst
173300070117     C                   eval      cmzfld = 'FIVADWWR'
173400040119     C                   eval      cmzmbd = vlrhdl
173500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173600040119     C***                if        prmfir = *blanks
173700070117     C                   eval      cmzfla = 'FIVAD00F'
173800070117     C                   eval      cmzmba = 'FIVAD00F'
173900040119     C***                else
174000040119     C***                eval      cmzfla = prmfir
174100040119     C***                eval      cmzmba = prmfir
174200040119     C***                endif
174300040119     C                   eval      cmznrr = *zeros
174400070117     C                   move      §ctrokvd      cmznrr
174500040119     C                   eval      cmzlba = vlrfl1
174600040119     C                   call(e)   'TIS711C'
174700040119     C                   parm                    dscmz
174800040119     C                   parm      *blanks       esito
174900040119     C                   if        %error
175000040119     C                             or cmzerr = '1'
175100040119     C                             or esito  = '1'
175200040119     C                   eval      wrkesito = '3'
175300040119     C                   else
175400040119     C*
175500081124     C* 3° invio FIVAB
175600040119     C                   reset                   dscmz
175700040119     C                   move      vlrpoi        cmzdst
175800040119     C                   eval      cmzfld = vlrfou
175900040119     C                   eval      cmzmbd = vlrhdl
176000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
176100040119     C***                if        prmfir = *blanks
176200040119     C                   eval      cmzfla = 'FIVAB00F'
176300040119     C                   eval      cmzmba = 'FIVAB00F'
176400040119     C***                else
176500040119     C***                eval      cmzfla = prmfir
176600040119     C***                eval      cmzmba = prmfir
176700040119     C***                endif
176800040119     C                   eval      cmznrr = *zeros
176900040119     C                   move      §ctrokvb      cmznrr
177000040119     C                   eval      cmzlba = vlrfl1
177100040119     C                   call(e)   'TIS711C'
177200040119     C                   parm                    dscmz
177300040119     C                   parm      *blanks       esito
177400040119     C                   if        %error
177500040119     C                             or cmzerr = '1'
177600040119     C                             or esito  = '1'
177700040119     C                   eval      wrkesito = '3'
177800040119     C                   endif
177900040119     C                   endif
178000081124     C                   endif
178100040119     C*
178200040119     C                   ENDSR
178300040119     C***
178400010601
178500010601
178600010601
178700010601
178800010601      /TITLE Invio dei dati al punto operativo.
178900010601     C     opeini        BEGSR
179000010601     C*
179100010601     C* Inizializzo flag e contatori operativi
179200010601     C                   movel     '0'           flgGiro           1
179300010601     C                   movel     '0'           flgMulti          1
179400010601     C                   movel     '1'           flgStato          1
179500010615     C                   movel     '0'           flgOk             1
179600010601     C                   z-add     *zeros        cntNonEl         10 0
179700010601     C                   z-add     *zeros        depfil            3 0
179800010601     C                   z-add     *zeros        invfil            3 0
179900010601     C*
180000010601     C                   ENDSR
180100010601     C***
180200010601
180300010601
180400050421
180500081006     C     *pssr         BEGSR
180600081006     C*
180700081006     C                   if        %open(tivin00r)
180800081006     C                   close     tivin00r
180900081006     C                   endif
181000081006     C                   if        %open(fivabwwr)
181100081006     C                   close     fivabwwr
181200081006     C                   endif
181300081006     C                   if        %open(fivadwwr)
181400081006     C                   close     fivadwwr
181500081006     C                   endif
181600081124     C                   if        %open(fivatwwr)
181700081124     C                   close     fivatwwr
181800081124     C                   endif
181900081006     C*
182000081006     C* Effettuo la chiamata al CLLE preposto
182100081124     C                   call(e)   'TITVVDTC'
182200081006     C                   parm                    parccm
182300081006     C                   parm                    parmbr
182400081006     C                   parm      '2'           paropz
182500081006     C*
182600050421     C                   eval      wrkesito = '2'
182700050421     C*
182800050421     C                   ENDSR     '*CANCL'
182900050421     C***
183000050421
183100010601
183200010330
183300010330
183400000613     C     *inzsr        BEGSR
183500990910     C*
183600990910     C     *entry        plist
183700990920     C                   parm                    tivlrds
183800990921     C                   parm      wrkesito      esito
183900000724     C                   parm                    prmlit
184000000710     C                   parm                    prmfir
184100010330     C*
184200010330     C* CALCOLA LA DATA CORRENTE
184300010330     C                   time                    wn14             14 0
184400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
184500010330     C                   z-add     wn8           g08dat
184600010330     C                   z-add     *zeros        g08inv
184700010330     C                   movel     '0'           g08err
184800010330     C                   call      'XSRDA8'
184900010330     C                   parm                    wlbda8
185000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
185100000613     C*
185200000613     C                   ENDSR
185300000613     C***
185400990908
185500040122     Otitv92p   E            riepilogo         2
185600990915     O                                              'Upload via Internet'
185700990915     O                                           +1 'Traduzione TIVIN00R -'
185800021025     O                                           +1 'FIVABWWR'
185900990915     O                                           +1 'Testate bolle'
186000990915     O          E            riepilogo   2
186100990915     O                       wrkdata
186200990915     O                       wrkora              +1
186300990915     O                       procname            +1
186400990915     O          E            riepilogo   2
186500990915     O                                              'Cliente..................:'
186600010605     O                       VABCCM        z     +1
186700990915     O          E            riepilogo   2
186800990920     O                                              'Riferimento Strategi.....:'
186900990920     O                       vlrhdl              +1
187000990915     O          E            riepilogo   2
187100990915     O                                              'Giusti...................:'
187200040119     O                       §CTROKVB      2   +  1
187300990915     O          E            riepilogo   2
187400990915     O                                              'Sbagliati e corretti.....:'
187500971022     O                       §CTRMO        2   +  1
187600990915     O          E            riepilogo   2
187700990915     O                                              'Sbagliati e scartati.....:'
187800971022     O                       §CTRNO        2   +  1
187900000613
188000040122     Otitv92ps  E            testdett          2
188100000613     O                                              'Upload via Internet'
188200000613     O                                           +1 'Traduzione TIVIN00R -'
188300021025     O                                           +1 'FIVABWWR'
188400000613     O                                           +1 'Testate bolle'
188500000616     O          E            testdett    3
188600000613     O                                           +2 'N° rec'
188700000613     O                                           +3 'Anteprima contenuto'
188800000616     O          E            rigadett    2
188900000613     O                       rrnum               +2
189000000621     O                       recko               +3
189100000616     O          E            findett     2
189200000613     O                       wrkdata
189300000613     O                       wrkora              +1
189400000613     O                       procname            +1
189500000616     O          E            findett     2
189600000613     O                                              'Cliente..................:'
189700010605     O                       VABCCM        z     +1
189800000616     O          E            findett     2
189900000613     O                                              'Riferimento Strategi.....:'
190000000613     O                       vlrhdl              +1
190100000616     O          E            findett     2
190200000613     O                                              'Giusti...................:'
190300040119     O                       §CTROKVB      2   +  1
190400000616     O          E            findett     2
190500000613     O                                              'Sbagliati e corretti.....:'
190600000613     O                       §CTRMO        2   +  1
190700000616     O          E            findett     2
190800000613     O                                              'Sbagliati e scartati.....:'
190900000613     O                       §CTRNO        2   +  1
191000000613** CMD - COMANDI CL
191100040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
191200040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
191300040122DLTOVR FILE(TITV92P TITV92PS) LVL(*)
191400010606
191500000613
191600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
191700040510
191800040122
191900040113
