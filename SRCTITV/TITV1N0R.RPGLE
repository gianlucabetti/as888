000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800090220     Ftitv1n0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100090220     Ftitv1n0ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300080916     F                                     infsr(*pssr)
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
004600080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
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
006200090220     D wRSD            s                   LIKE(VABRSD)
006300090220     D wRSD2           s                   LIKE(VABRSD)
006400090220     D wIND            s                   LIKE(VABIND)
006500090220     D wIND2           s                   LIKE(VABIND)
006600090220     D wCAD            s                   LIKE(VABCAD)
006700090220     D wCAD2           s                   LIKE(VABCAD)
006800090220     D wLOD            s                   LIKE(VABLOD)
006900090220     D wLOD2           s                   LIKE(VABLOD)
007000090220     D wPRD            s                   LIKE(VABPRD)
007100090220     D wPRD2           s                   LIKE(VABPRD)
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
009200080617     D*------------------
009300080617
009400080617     D*------------------
009500080617     D* LINKING A DEFINIZIONI ESTERNE
009600080617     D*------------------
009700080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009800090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009900010330
010000010330
010100010330
010200990915     C                   time                    wrkdata
010300990915     C                   time                    wrkora
010400000913     C                   reset                   rrnum
010500990921     C                   reset                   esito
010600081014     C                   reset                   wrkesito
010700010601     C*
010800010601     C                   exsr      opeini
010900010605     C                   exsr      rwvab
011000081014     C*
011100081014     C                   exsr      endela
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
012800000724     C*
012900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013100010601     C     invfil        chain     azorg01l
013200000724     C                   if        %found
013300000724     C                   movel(p)  CMD(1)        depcmd
013400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013500000724     C*
013600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013700011113     C                   Z-ADD     150           LENGH            15 5
013800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013900000724     C                   PARM                    depcmd
014000000724     C                   PARM                    LENGH
014100000724     C*
014200000724     C                   endif
014300000616     C*
014400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014500000616     C                   movel(p)  CMD(2)        depcmd
014600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014700000616     C*
014800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014900011113     C                   Z-ADD     150           LENGH            15 5
015000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015100000616     C                   PARM                    depcmd
015200000616     C                   PARM                    LENGH
015300000613     C*
015400090220     C                   if        not %open(titv1n0ps)
015500090220     C                   open      titv1n0ps
015600070329     C                   except    testdett
015700000613     C                   endif
015800010601     C*
015900010601     C                   endif
016000010601     C*
016100010601     C                   ENDSR
016200010601     C***
016300010601
016400010601
016500010601
016600010601
016700010601     C*--------------------------------------------------------
016800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016900010601     C*--------------------------------------------------------
017000010601     C     ENDELA        BEGSR
017100081014     C*
017200081014     C                   if        %open(tivin00r)
017300081014     C                   close     tivin00r
017400081014     C                   endif
017500081014     C                   if        %open(fivabwwr)
017600081014     C                   close     fivabwwr
017700081014     C                   endif
017800081014     C                   if        %open(fivatwwr)
017900081014     C                   close     fivatwwr
018000081014     C                   endif
018100040119     C*
018200040119     C* Effettuo la chiamata al CLLE preposto
018300040119     C                   call(e)   'TITVVTC'
018400040510     C                   parm                    parccm
018500040119     C                   parm                    parmbr
018600040119     C                   parm      '2'           paropz
018700050201     C*
018800050201     C* Effettuo lancio TISI95 solo x chiusura
018900050201     C                   CLEAR                   TISI95DS
019000050201     C                   EVAL      I95TLA = 'C'
019100050201     C                   CALL      'TISI95R'
019200050201     C                   PARM                    TISI95DS
019300000613     C*
019400090220     C                   if        %open(titv1n0ps)
019500000616     C                   except    findett
019600090220     C                   close     titv1n0ps
019700000613     C                   endif
019800000616     C*
019900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020000011113     C                   Z-ADD     150           LENGH            15 5
020100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020200000616     C                   PARM                    CMD(3)
020300000616     C                   PARM                    LENGH
020400000616     C*
020500010601     C                   ENDSR
020600010601     C***
020700010601
020800010601
020900010601
021000000613
021100000613     C*--------------------------------------------------------
021200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021300000613     C*--------------------------------------------------------
021400000621     C     STPR          BEGSR
021500000613     C*
021600090220     C                   if        not %open(titv1n0p)
021700090220     C   50              open      titv1n0p
021800990915     C                   endif
021900990915     C*
022000030715     C   50              except    riepilogo
022100990915     C*
022200090220     C                   if        %open(titv1n0p)
022300090220     C   50              close     titv1n0p
022400990914     C                   endif
022500000613     C*
022600000613     C                   ENDSR
022700000613     C***
022800000613
022900010601
023000010601
023100010330     C*--------------------------------------------------------
023200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023300010330     C*--------------------------------------------------------
023400010605     C     RWVAB         BEGSR
023500010330     C*
023600010330     C                   if        not %open(tivin00r)
023700010330     C                   open      tivin00r
023800010330     C                   endif
023900021025     C                   if        not %open(fivabwwr)
024000021025     C                   open      fivabwwr
024100010330     C                   endif
024200040119     C*
024300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024400040119     C                   exsr      prevat
024500040119     C*
024600040119     C                   if        chkcall = '0'
024700040119     C*
024800040119     C                   if        not %open(fivatwwr)
024900040119     C                   open      fivatwwr
025000040119     C                   endif
025100010330     C*
025200040119     C                   clear                   §CTROKVB          7 0
025300040119     C                   clear                   §CTROKVT          7 0
025400040119     C                   clear                   §CTRMO            7 0
025500040119     C                   clear                   §CTRNO            7 0
025600070823     C*
025700070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025800070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025900081016     C***                exsr      inzvar
026000081016     C***                exsr      impvab
026100010330     C*
026200030822     C                   do        *HIVAL
026300010330     C*
026400030822     C                   read      tivin00r                               70
026500010618     C*
026600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026700010618     C                   if        vinflg = '1'
026800010618     C                   eval      flgOk = '1'
026900010618     C                   endif
027000040510     C*
027100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027200040510     C                   z-add     *zeros        wLenVINDTA        4 0
027300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027700040510     C                   z-add     1             wFlgVINDTA
027800040510     C                   leave
027900040510     C                   endif
028000040510     C                   enddo
028100010618     C*
028200040510     C                   if        vindta > *blanks AND
028300040510     C                             wFlgVINDTA = 1
028400010330     C                   add       1             rrnum
028500010330     C*
028600010601     C                   if        *in70 = *off and
028700010330     C                             (vinflg = *blanks
028800010330     C                              or vinflg = '0'
028900010330     C                              or vinflg = '2')
029000010330     C*
029100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029200010711     C                   if        vinflg = *blanks or vinflg = '0'
029300010711     C                   clear                   vinmsg
029400010711     C                   endif
029500010601     C*
029600070925     C* Aggiungo un byte a blanks all'inizio d ogni record
029700070925     C                   eval      vindta  = ' ' + %trim(vindta)
029800070925     C*
029900010330     C                   exsr      inzvar
030000010605     C                   exsr      impvab
030100010601     C*
030200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030300010605     C                   eval      depfil = VABLNP
030400010601     C                   exsr      repfil
030500010601     C                   if        depfil = invfil
030600021025     C                   if        vlrpoi = 999
030700030822     C                   move(P)   invfil        VABFGS
030800021025     C                   else
030900030822     C                   move(P)   vlrpoi        VABFGS
031000021025     C                   endif
031100010601     C*
031200010601     C                   exsr      PREELA
031300010601     C*
031400010604     C* Ebbene...
031500010604     C*
031600040119     C  N31              add       1             §CTROKVB
031700040119     C   32              add       1             §CTRMO
031800040119     C   31              add       1             §CTRNO
031900030822     C                   if        wGiro = 2
032000040510     C* Scrivo sempre il VAT
032100040510     C  N31              exsr      wrivat
032200040510     C* Scrivo il VAB solo a rottura d spedizione
032300040119     C                   if        depspe =  *zeros OR
032400040119     C                             depspe <> vabNSP
032500040119     C  N31              exsr      wrivab
032600040119     C  N31              eval      depspe = vabNSP
032700040119     C                   endif
032800040119     C                   endif
032900010604     C*
033000010604     C                   if        *in31 = *off and
033100010604     C                             *in32 = *off
033200010604     C                   eval      vinflg = '1'
033300010604     C                   else
033400010604     C                   eval      recko = vindta
033500010604     C                   except    rigadett
033600010604     C                   eval      vinflg = '2'
033700010604     C                   endif
033800010604     C                   endif
033900010601     C*
034000010604     C                   endif
034100010604     C*
034200010330     C                   else
034300010330     C                   eval      vinflg = '1'
034400010330     C                   endif
034500010601     C*
034600010601     C  N70              update    tivin000
034700010330     C*
034800030822     C  N70              enddo
034900040119     C*
035000040119     C                   endif                                                  (endif - chkcall)
035100010601     C*
035200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035300010601     C                   if        cntNonEl = *zeros or
035400010601     C                             flgMulti = '0'
035500010330     C* Se non ci sono record con errori ...
035600010601     C                   if        §ctrno = 0 and
035700010604     C                             §ctrmo = 0 and
035800010601     C                             flgStato <> '2'
035900010330     C* ... restituisco esito OK.
036000010330     C                   eval      wrkesito = '0'
036100010330     C                   else
036200040119     C                   if        §ctrokvb > 0
036300010330     C                   eval      wrkesito = '1'
036400010330     C                   else
036500010615     C                   if        flgOk = '0'
036600010615     C                   eval      wrkesito = '2'
036700010615     C                   else
036800010615     C                   eval      wrkesito = '6'
036900010615     C                   endif
037000010330     C                   endif
037100010330     C                   endif
037200010601     C                   else
037300010601     C                   eval      wrkesito = '9'
037400010601     C                   endif
037500010330     C*
037600010330     C                   if        %open(tivin00r)
037700010330     C                   close     tivin00r
037800010330     C                   endif
037900021025     C                   if        %open(fivabwwr)
038000021025     C                   close     fivabwwr
038100010330     C                   endif
038200040512     C                   if        %open(fivatwwr)
038300040512     C                   close     fivatwwr
038400040512     C                   endif
038500010601     C*
038600010601     C                   if        vlrpoi <> 999
038700010601     C                   eval      invfil = vlrpoi
038800010601     C                   endif
038900010330     C*
039000040119     C                   if        §ctrokvb > 0
039100010601     C                             and invfil > *zeros
039200010330     C                   exsr      invio
039300010330     C                   endif
039400010601     C*
039500010618     C                   if        flgGiro = '1'
039600081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039700010618     C                   endif
039800010330     C*
039900010330     C                   ENDSR
040000010330     C***
040100040119
040200040119
040300040119
040400040119     C*----------------------------------------------------*
040500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040600040119     C*----------------------------------------------------*
040700040510     C     WRIVAB        BEGSR
040800040119     C*
040900060331     C* Gestisco l'eventuale rottura x numero spedizione
041000070823     C                   if        VABNSP <> depspe
041100070301     C                   if        wVATNOT_E <> *blanks
041200080125     C                   if        VABCTM = *blanks
041300080125     C                   eval      VABCTM = '7Q'
041400080125     C                   endif
041500070301     C                   endif
041600040119     C                   write     fivab000                                     => scarico il VAB
041700060331     C                   endif
041800040119     C*
041900040119     C                   ENDSR
042000040119
042100040119
042200040119
042300040119     C*----------------------------------------------------*
042400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042500040119     C*----------------------------------------------------*
042600040510     C     WRIVAT        BEGSR
042700060512     C*
042800060512     C* Inizializzo i campi chiave
042900060512     C                   eval      VATFGS = VABFGS
043000060512     C                   eval      VATAAS = VABAAS
043100060512     C                   eval      VATCCM = VABCCM
043200070823     C                   eval      VATNRS = VABNRS
043300060512     C                   eval      VATNSP = VABNSP
043400060512     C                   eval      VATLNP = VABLNP
043500060331     C*
043600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043700060331     C                   if        wVATNOT_A <> *blanks
043800060331     C                   eval      VATNOT = wVATNOT_A
043900060331     C                   eval      VATTRC = 'A'
044000060331     C                   write     FIVAT000
044100060331     C                   add       1             §CTROKVT
044200060331     C                   endif
044300060331     C*
044400060331     C                   if        wVATNOT_B <> *blanks
044500060331     C                   eval      VATNOT = wVATNOT_B
044600060331     C                   eval      VATTRC = 'B'
044700060331     C                   write     FIVAT000
044800060331     C                   add       1             §CTROKVT
044900060331     C                   endif
045000060331     C*
045100080625     C                   if        wVATNOT_E <> *blanks
045200060331     C                   eval      VATNOT = wVATNOT_E
045300060331     C                   eval      VATTRC = 'E'
045400080716     C                   write     FIVAT000
045500080716     C                   add       1             §CTROKVT
045600080625     C                   endif
045700040119     C*
045800040119     C                   ENDSR
045900040119
046000040119
046100040119
046200040119     C*----------------------------------------------------*
046300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046400040119     C*----------------------------------------------------*
046500040119     C     PREVAT        BEGSR
046600040119     C*
046700040119     C* Compongo il nome del membro da dare al FIVATWWR
046800040119     C                   eval      parmbr = vlrhdl
046900040119     C                   movel     'M'           parmbr
047000040510     C                   eval      parccm = vlrksc
047100040119     C                   eval      paropz = '1'
047200040119     C* Effettuo la chiamata al CLLE preposto
047300040119     C                   call(e)   'TITVVTC'
047400040119     C                   parm                    parccm
047500040119     C                   parm                    parmbr
047600040119     C                   parm                    paropz
047700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047800040119     C                   if        %error
047900040119     C                   movel     '1'           chkcall
048000040119     C                   else
048100040119     C                   movel     '0'           chkcall
048200040119     C                   endif
048300040119     C*
048400040119     C                   ENDSR
048500010601
048600010601
048700010601
048800010330     C*----------------------------------------------------*
048900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049000010330     C*----------------------------------------------------*
049100040119     C     INZVAR        BEGSR
049200010330     C*
049300030822     C* Inizializzo variabili di wrk
049400020204     C                   Z-ADD     *zeros        Num5_0            5 0
049500060331     C                   MOVEL     *blanks       wVATNOT_A
049600060331     C                   MOVEL     *blanks       wVATNOT_B
049700060331     C                   MOVEL     *blanks       wVATNOT_E
049800090220     C*
049900090220     C                   MOVEL     *blanks       wRSD
050000090220     C                   MOVEL     *blanks       wRSD2
050100090220     C                   MOVEL     *blanks       wIND
050200090220     C                   MOVEL     *blanks       wIND2
050300090220     C                   MOVEL     *blanks       wCAD
050400090220     C                   MOVEL     *blanks       wCAD2
050500090220     C                   MOVEL     *blanks       wLOD
050600090220     C                   MOVEL     *blanks       wLOD2
050700090220     C                   MOVEL     *blanks       wPRD
050800090220     C                   MOVEL     *blanks       wPRD2
050900090220     C*
051000080617     C                   EVAL      wFlgCAS = *blanks
051100030822     C*
051200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051300030822     C                   CLEAR                   FIVAB000
051400040119     C                   CLEAR                   FIVAT000
051500030822     C                   CLEAR                   SkSplitCSV
051600030822     C                   EVAL      i = 1
051700030822     C                   EVAL      posDa = *zeros
051800030822     C                   EVAL      posA  = *zeros
051900030822     C*
052000030822     C* Reimposto i valori di default
052100030822     C                   EXSR      DEFCAM
052200030822     C*
052300010330     C                   ENDSR
052400010330     C*----------------------------------------------------*
052500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052600010330     C*----------------------------------------------------*
052700010330     C     DEFCAM        BEGSR
052800030822     C*
052900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053000030715     C* e delimitatore testo.
053100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053400030822     C*
053500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
053700030715     C                   SETON                                        50
053800030715     C                   ELSE
053900030715     C                   SETOFF                                       50
054000030715     C                   ENDIF
054100030822     C*
054200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054300030822     C                   EVAL      posDaDft = 1
054400030822     C                   EVAL      posADft  = 0
054500030822     C                   EVAL      wGiroDft = 0
054600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054700030822     C                             posDaDft > 0
054800030822     C*
054900030822     C* Gestisco il 1° giro
055000030822     C                   IF        wGiroDft = 0
055100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055300030822     C* Incremento il contatore dei "giri"
055400030822     C                   EVAL      wGiroDft = 1
055500030822     C                   ELSE
055600030822     C                   EVAL      posDaDft = posADft
055700030822     C                   ENDIF
055800030822     C* Eseguo lo scan x trovare la fine del campo corrente
055900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056000030822     C*
056100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056300030822     C                   IF        posDaDft > 0 AND
056400030822     C                             posADft  > 0
056500030822     C* NCL
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'NCL'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABNCL
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* CCM
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'CCM'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABCCM
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* LNP
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'LNP'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABLNP
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* NRS
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'NRS'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABNRS
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* CTR
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'CTR'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABCTR
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* PKB
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'PKB'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000030822     C                   Z-ADD     PiVal         VABPKB
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* VLB
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'VLB'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiNum=*on
065300030822     C                   Z-ADD     PiVal         VABVLB
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* QFT
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'QFT'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiNum=*on
066600030822     C                   Z-ADD     PiVal         VABQFT
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* CBO
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'CBO'
067400030822     C                   EVAL      VABCBO=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* TSP
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'TSP'
068300030822     C                   EVAL      VABTSP=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* VAS
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'VAS'
069200030822     C                   EVAL      VABVAS=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* VCA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VCA'
070100030822     C                   EVAL      VABVCA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500070925     C* TIC
070600070925     C                   IF        %subst(
070700070925     C                             %subst(vlrppt:posDaDft+1:
070800070925     C                             posADft-posDaDft-1):1:3)
070900070925     C                             = 'TIC'
071000070925     C                   EVAL      VABTIC=%trim(%subst(
071100070925     C                             %subst(vlrppt:posDaDft+1:
071200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300070925     C                   ENDIF
071400030822     C* GCA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GCA'
071900030822     C                   EVAL      VABGCA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* CTM
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'CTM'
072800030822     C                   EVAL      VABCTM=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* FFD
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'FFD'
073700030822     C                   EVAL      VABFFD=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* VAD
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'VAD'
074600030822     C                   EVAL      VABVAD=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* GMA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GMA'
075500030822     C                   EVAL      VABGMA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* GGA
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'GGA'
076400030822     C                   EVAL      VABGGA=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* GVA
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'GVA'
077300030822     C                   EVAL      VABGVA=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* TC1
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'TC1'
078200030822     C                   EVAL      VABTC1=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* TC2
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'TC2'
079100030822     C                   EVAL      VABTC2=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500040714     C* VATTRC
079600040714     C                   IF        %subst(
079700040714     C                             %subst(vlrppt:posDaDft+1:
079800040714     C                             posADft-posDaDft-1):1:3)
079900040714     C                             = 'TRC'
080000040714     C                   EVAL      VATTRC=%trim(%subst(
080100040714     C                             %subst(vlrppt:posDaDft+1:
080200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300040714     C                   ENDIF
080400030822     C* ...
080500030822     C                   ENDIF
080600030822     C                   ENDDO
080700020204     C*
080800010330     C                   ENDSR
080900010607     C*----------------------------------------------------*
081000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081100010607     C*----------------------------------------------------*
081200010607     C     IMPVAB        BEGSR
081300010607     C*
081400010607     C                   SETOFF                                       3132
081500030515     C*
081600010607     C                   MOVEL     datcor        VABAAS
081700010607     C                   MOVE      datcor        VABMGS
081800010607     C*
081900030715     C**********
082000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082100030715     C**********
082200030715     C                   IF        wGiro = *zeros
082300080716     C*
082400080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
082500080716     C                   EVAL      depvinDTA = vinDTA
082600040113     C*
082700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082900040510     C                             %trim(intcol(3))<>*blanks
083000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083100040113     C                                      %trim(intcol(3))
083200070823     C     ';':CharCSV   XLATE     vindta        vindta
083300040510     C                   ENDIF
083400080626     C*
083500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083600031201     C     minu:maiu     XLATE     vindta        vindta
083700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083800030715     C                   DOW       posDa <= %len(%trim(vindta))
083900030715     C*
084000030715     C* Gestisco il 1° campo
084100030715     C                   IF        i = 1
084200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084300030715     C                   EVAL      posDa = 1
084400030715     C* Eseguo lo scan x trovare la fine del primo campo
084500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C* X i campi successivi al 1°
085000030715     C                   ELSE
085100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085200030715     C                   EVAL      posDa = posA + 1
085300030715     C* Eseguo lo scan x trovare la fine del campo corrente
085400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085500030715     C* Gestisco l'ultimo campo
085600030715     C                   IF        posA = *zeros
085700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085800030715     C* A questo punto "estraggo" il campo corrente
085900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086000030715     C                                                    (posA-posDa))
086100030715     C                   LEAVE
086200030715     C                   ELSE
086300030715     C* A questo punto "estraggo" il campo corrente
086400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086500030715     C                                                    (posA-posDa))
086600030715     C                   ENDIF
086700030715     C                   ENDIF
086800030715     C* Incremento il contatore d campo
086900030715     C                   EVAL      i = i +1
087000030715     C                   ENDDO
087100030715     C*
087200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087300030715     C* delimitazione testo
087400030715     C                   EVAL      i = 1
087500030715     C                   DOW       i < %elem(SkSplitFLD)
087600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087800030715     C                   EVAL      i = i + 1
087900030715     C                   ENDDO
088000030715     C                   EVAL      wGiro = 1
088100080716     C* Ripristino la 1° riga originale
088200080716     C                   EVAL      vinDTA = depvinDTA
088300030715     C                   ELSE
088400030715     C                   EVAL      wGiro = 2
088500031204     C**********
088600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088700031204     C* d separatore campo
088800031204     C**********
088900031204     C                   EVAL      vindta = %trim(vindta)
089000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089100031204     C                   EVAL      vindta = %subst(vindta:2)
089200031204     C                   ENDDO
089300031204     C*
089400031204     C                   Z-ADD     *zeros        lunghInput        4 0
089500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089900031204     C                   ENDDO
090000030715     C**********
090100030715     C* Effettuo lo split del campo dati d input
090200030715     C**********
090300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090400030715     C                   DOW       posDa <= %len(%trim(vindta))
090500030715     C*
090600030715     C* Gestisco il 1° campo
090700030715     C                   IF        i = 1
090800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090900030715     C                   EVAL      posDa = 1
091000030715     C* Eseguo lo scan x trovare la fine del primo campo
091100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C* X i campi successivi al 1°
091600030715     C                   ELSE
091700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091800030715     C                   EVAL      posDa = posA + 1
091900031204     C* Verifico che nn vi sia il campo nn valorizzato
092000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092100031204     C* Se campo nn valorizzato skippo al prossimo
092200031204     C                   EVAL      SkSplitCSV(i) = *blanks
092300031204     C                   EVAL      posA  = posA + 1
092400031204     C                   ELSE
092500030715     C* Eseguo lo scan x trovare la fine del campo corrente
092600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092700030715     C* Gestisco l'ultimo campo
092800030715     C                   IF        posA = *zeros
092900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093000030715     C* A questo punto "estraggo" il campo corrente
093100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093200030715     C                                                    (posA-posDa))
093300030715     C                   LEAVE
093400030715     C                   ELSE
093500030715     C* A questo punto "estraggo" il campo corrente
093600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093700030715     C                                                    (posA-posDa))
093800030715     C                   ENDIF
093900031204     C                   ENDIF
094000030715     C                   ENDIF
094100030715     C* Incremento il contatore d campo
094200030715     C                   EVAL      i = i +1
094300030715     C                   ENDDO
094400030715     C*
094500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094600030715     C* delimitazione testo
094700030715     C                   EVAL      i = 1
094800030715     C                   DOW       i < %elem(SkSplitCSV)
094900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095100030715     C                   EVAL      i = i + 1
095200030715     C                   ENDDO
095300030715     C*
095400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095500030715     C                   EVAL      i = 1
095600030715     C                   DOW       i < %elem(SkSplitFLD)
095700030715     C*
095800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096000030715     C                             SkSplitCSV(i) <> *blanks
096100030721     C*** ATB
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096300030721     C                   EVAL      VABATB = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030715     C*** RSD
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097000030715     C* ==
097100030715     C                   ENDIF
097200030721     C*** RD2
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030715     C*** IND
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097800030715     C                   EVAL      VABIND = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** LOD
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** PRD
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030721     C*** NZD
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030715     C*** RMA
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** NOT
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** NT2
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** VCA
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** VAS
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** FFD
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030721     C*** GC1
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** GC2
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** TSP
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102700030721     C                   ENDIF
102800040510     C*** CBO
102900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103100040510     C                   ENDIF
103200030721     C*** NAS
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** TIC
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** GCA
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** XCO
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** CTM
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** TCR
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** CTS
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** FTM
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** VAD
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GMA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GGA
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** GVA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TC1
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TC2
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** SCL
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** RMO
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** NMO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*
110100030721     C* Reperisco quindi i campi numerici...
110200040510     C*** NSP
110300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110400040510     C                   EVAL      PiStr=SkSplitCSV(i)
110500040510     C                   EXSR      CHKNUM
110600040510     C                   IF        PiInt=*on
110700040510     C                   Z-ADD     PiVal         VABNSP
110800040510     C                   ELSE
110900040510     C                   SETON                                        31
111000040510     C                   EVAL      VABNSP = *zeros
111100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111200040510     C                             + ' ' + 'VABNSP'
111300040510     C                   ENDIF
111400040510     C                   ENDIF
111500071106     C*** RMN
111600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111700071106     C                   EVAL      PiStr=SkSplitCSV(i)
111800071106     C                   EXSR      CHKNUM
111900071106     C                   IF        PiInt=*on
112000071106     C                   Z-ADD     PiVal         VABRMN
112100071106     C                   ELSE
112200071106     C                   SETON                                        32
112300071106     C                   EVAL      VABRMN = *zeros
112400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112500071106     C                             + ' ' + 'VABRMN'
112600071106     C                   ENDIF
112700071106     C                   ENDIF
112800030721     C*** CAD
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on
113300030721     C                   Z-ADD     PiVal         Num5_0
113400030721     C                   MOVEL(p)  Num5_0        VABCAD
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      VABCAD = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABCAD'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** NCL
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on
114700030721     C                   Z-ADD     PiVal         VABNCL
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      VABNCL = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200040122     C                             + ' ' + 'VABNCL'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** PKB
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiNum=*on
116000030721     C                   Z-ADD     PiVal         VABPKB
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABPKB = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABPKB'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** CAS
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117000080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
117100080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
117200081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
117300081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
117400080125     C                   EVAL      wFlgCAS = '1'
117500080125     C                   ENDIF
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiNum=*on
117900030721     C                   Z-ADD     PiVal         VABCAS
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      VABCAS = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABCAS'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** CCM
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on      AND
119200030721     C                             PiVal<=9999999 AND
119300030721     C                             PiVal>*zeros
119400030721     C                   Z-ADD     PiVal         VABCCM
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      VABCCM = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040510     C                             + ' ' + 'VABCCM'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** LNP
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on    AND
120700030721     C                             PiVal<=999   AND
120800030721     C                             PiVal>*zeros
120900030721     C                   Z-ADD     PiVal         VABLNP
121000030721     C                   ELSE
121100040715     C                   SETON                                        32
121200030721     C                   EVAL      VABLNP = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400040510     C                             + ' ' + 'VABLNP'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** NRS
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiInt=*on AND
122200030721     C                             PiVal<=99
122300030721     C                   Z-ADD     PiVal         VABNRS
122400030721     C                   ELSE
122500030721     C                   SETON                                        32
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABNRS'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** LNA
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on    AND
123500030721     C                             PiVal<=999   AND
123600030721     C                             PiVal>*zeros
123700030721     C                   Z-ADD     PiVal         VABLNA
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      VABLNA = *zeros
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABLNA'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** CTR
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on  AND
125000030721     C                             PiVal<=999
125100030721     C                   Z-ADD     PiVal         VABCTR
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABCTR'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** IAS
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiNum=*on
126300030721     C                   Z-ADD     PiVal         VABIAS
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABIAS'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** VLB
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiNum=*on
127500030721     C                   Z-ADD     PiVal         VABVLB
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABVLB'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** QFT
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiNum=*on
128700030721     C                   Z-ADD     PiVal         VABQFT
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABQFT'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** NCD
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on      AND
129900030721     C                             PiVal<=9999999
130000030721     C                   Z-ADD     PiVal         VABNCD
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABNCD'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** NCA
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on      AND
131200030721     C                             PiVal<=9999999
131300030721     C                   Z-ADD     PiVal         VABNCA
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABNCA'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** ZNC
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on AND
132500030721     C                             PiVal<=99
132600030721     C                   Z-ADD     PiVal         VABZNC
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABZNC'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** DCR
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on       AND
133800030721     C                             PiVal<=99999999
133900030721     C                   Z-ADD     PiVal         VABDCR
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABDCR'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** HCR
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on   AND
135100030721     C                             PiVal<=9999
135200030721     C                   Z-ADD     PiVal         VABHCR
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABHCR'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** VMD
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiNum=*on
136400030721     C                   Z-ADD     PiVal         VABVMD
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABVMD'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** ANT
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiInt=*on        AND
137600030721     C                             PiVal<=999999999
137700030721     C                   Z-ADD     PiVal         VABANT
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABANT'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** CMO
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiInt=*on
138900030721     C                   Z-ADD     PiVal         Num5_0
139000030721     C                   MOVEL(p)  Num5_0        VABCMO
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      VABCMO = *zeros
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500030721     C                             + ' ' + 'VABCMO'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800040510     C*
139900040510     C* Reperisco i campi del FIVAT
140000040510     C*** VATNOT
140100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140300040510     C                   ENDIF
140400040510     C*** VATTRC
140500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140700040510     C                   ENDIF
140800060331     C*** VATNOT_A
140900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141100060331     C                   ENDIF
141200060331     C*** VATNOT_B
141300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141500060331     C                   ENDIF
141600060331     C*** VATNOT_E
141700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141900060331     C                   ENDIF
142000070823     C*
142100071029     C***  ===>  Gestione campi "particolari" dentro ciclo
142200090220     C*
142300090220     C*** wRSD
142400090220     C                   IF        %trim(SkSplitFLD(i)) = 'WRSD'
142500090220     C                   EVAL      wRSD = %trim(SkSplitCSV(i))
142600090220     C                   ENDIF
142700090220     C*** wIND
142800090220     C                   IF        %trim(SkSplitFLD(i)) = 'WIND'
142900090220     C                   EVAL      wIND = %trim(SkSplitCSV(i))
143000090220     C                   ENDIF
143100090220     C*** wCAD
143200090220     C                   IF        %trim(SkSplitFLD(i)) = 'WCAD'
143300090220     C                   EVAL      wCAD = %trim(SkSplitCSV(i))
143400090220     C                   ENDIF
143500090220     C*** wLOD
143600090220     C                   IF        %trim(SkSplitFLD(i)) = 'WLOD'
143700090220     C                   EVAL      wLOD = %trim(SkSplitCSV(i))
143800090220     C                   ENDIF
143900090220     C*** wPRD
144000090220     C                   IF        %trim(SkSplitFLD(i)) = 'WPRD'
144100090220     C                   EVAL      wPRD = %trim(SkSplitCSV(i))
144200090220     C                   ENDIF
144300090220     C*
144400090220     C*** wRSD2
144500090220     C                   IF        %trim(SkSplitFLD(i)) = 'WRSD2'
144600090220     C                   EVAL      wRSD2 = %trim(SkSplitCSV(i))
144700090220     C                   ENDIF
144800090220     C*** wIND2
144900090220     C                   IF        %trim(SkSplitFLD(i)) = 'WIND2'
145000090220     C                   EVAL      wIND2 = %trim(SkSplitCSV(i))
145100090220     C                   ENDIF
145200090220     C*** wCAD2
145300090220     C                   IF        %trim(SkSplitFLD(i)) = 'WCAD2'
145400090220     C                   EVAL      wCAD2 = %trim(SkSplitCSV(i))
145500090220     C                   ENDIF
145600090220     C*** wLOD2
145700090220     C                   IF        %trim(SkSplitFLD(i)) = 'WLOD2'
145800090220     C                   EVAL      wLOD2 = %trim(SkSplitCSV(i))
145900090220     C                   ENDIF
146000090220     C*** wPRD2
146100090220     C                   IF        %trim(SkSplitFLD(i)) = 'WPRD2'
146200090220     C                   EVAL      wPRD2 = %trim(SkSplitCSV(i))
146300090220     C                   ENDIF
146400070925     C*
146500071029     C***  <===  -----------------------------------------
146600040510     C*
146700030715     C                   ENDIF
146800030715     C                   EVAL      i = i + 1
146900030715     C                   ENDDO
147000070925     C*
147100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147200080125     C*
147300090220     C                   IF        wRSD2 <> *blanks
147400090220     C                   EVAL      VABRSD = wRSD2
147500090220     C                   ELSE
147600090220     C                   EVAL      VABRSD = wRSD
147700090220     C                   ENDIF
147800090220     C*
147900090220     C                   IF        wIND2 <> *blanks
148000090220     C                   EVAL      VABIND = wIND2
148100090220     C                   ELSE
148200090220     C                   EVAL      VABIND = wIND
148300090220     C                   ENDIF
148400090220     C*
148500090220     C                   IF        wCAD2 <> *blanks
148600090220     C                   EVAL      VABCAD = wCAD2
148700090220     C                   ELSE
148800090220     C                   EVAL      VABCAD = wCAD
148900090220     C                   ENDIF
149000090220     C*
149100090220     C                   IF        wLOD2 <> *blanks
149200090220     C                   EVAL      VABLOD = wLOD2
149300090220     C                   ELSE
149400090220     C                   EVAL      VABLOD = wLOD
149500090220     C                   ENDIF
149600090220     C*
149700090220     C                   IF        wPRD2 <> *blanks
149800090220     C                   EVAL      VABPRD = wPRD2
149900090220     C                   ELSE
150000090220     C                   EVAL      VABPRD = wPRD
150100090220     C                   ENDIF
150200070925     C*
150300071029     C***  <===  -----------------------------------------
150400070925     C*
150500070925     C* Considerazioni finali su CBO/CAS
150600080125     C                   IF        wFlgCAS = '1'
150700070925     C                   IF        VABCBO = '1'
150800070925     C                   EVAL      VABCBO = '4'
150900070925     C                   ENDIF
151000070925     C                   IF        VABCBO = '2'
151100070925     C                   EVAL      VABCBO = '6'
151200070925     C                   ENDIF
151300070925     C                   ENDIF
151400070925     C***  <===  ----------------------------
151500040510     C*
151600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151700040510     C                   IF        VABNSP = *zeros
151800040510     C                   SETON                                        31
151900040510     C                   ENDIF
152000040715     C*
152100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
152200040715     C                   IF        VABLNP = *zeros
152300040715     C                   SETON                                        31
152400040715     C                   ENDIF
152500040510     C*
152600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152700040510     C                   IF        VABRMN = *zeros
152800040510     C                   Z-ADD     VABNSP        VABRMN
152900040510     C                   ENDIF
153000040510     C*
153100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153200040510     C                   IF        VABRMA = *blanks
153300040714     C                   MOVEL     VABRMN        VABRMA
153400040510     C                   ENDIF
153500040128     C*
153600040510     C* Se provincia nn valorizzata la reperisco
153700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153800040510     C                   IF        VABPRD = *blanks
153900040128     C                   CLEAR                   TISI95DS
154000040128     C                   EVAL      I95TCN = '3'
154100040128     C                   Z-ADD     datcor        I95DAT
154200040128     C                   EVAL      I95NAR = VABNZD
154300040128     C                   EVAL      I95CAP = VABCAD
154400040128     C                   EVAL      I95LOC = VABLOD
154500040128     C                   CALL      'TISI95R'
154600040128     C                   PARM                    TISI95DS
154700040128     C                   EVAL      VABPRD = O95PRV
154800040510     C                   ENDIF
154900020204     C*
155000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155100020204     C                   EXSR      CHKIMPDIV
155200030715     C                   ENDIF
155300020204     C*
155400010607     C                   ENDSR
155500010607     C*----------------------------------------------------*
155600010601
155700020204
155800020204     C*----------------------------------------------------*
155900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156000020204     C*----------------------------------------------------*
156100020204     C     CHKIMPDIV     BEGSR
156200020204     C*
156300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156400020204     C                   Z-ADD     *zeros        wrkDec            9 9
156500020204     C*
156600020204     C* Come prima cosa effettuo considerazioni sulla divisa
156700020204     C                   IF        vabIAS > *zeros
156800020204     C                   IF        vabVAS <> 'EUR'
156900020204     C                   EVAL      vabVAS =  'ITL'
157000020204     C                   ENDIF
157100020204     C                   ENDIF
157200020204     C*
157300020204     C                   IF        vabCAS > *zeros
157400020204     C                   IF        vabVCA <> 'EUR'
157500020204     C                   EVAL      vabVCA =  'ITL'
157600020204     C                   ENDIF
157700020204     C                   ENDIF
157800020204     C*
157900020204     C                   IF        vabVMD > *zeros
158000020204     C                   IF        vabVAD <> 'EUR'
158100020204     C                   EVAL      vabVAD =  'ITL'
158200020204     C                   ENDIF
158300020204     C                   ENDIF
158400020204     C*
158500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
158600020204     C                   Z-ADD     vabIAS        wrkDec
158700020204     C                   IF        wrkDec > *zeros
158800020204     C                   IF        vabVAS = 'ITL'
158900020204     C                   EVAL      vabIAS = *zeros
159000020204     C                   ENDIF
159100020204     C                   ENDIF
159200020204     C*
159300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
159400020204     C                   Z-ADD     vabCAS        wrkDec
159500020204     C                   IF        wrkDec > *zeros
159600020204     C                   IF        vabVCA = 'ITL'
159700020204     C                   EVAL      vabCAS = *zeros
159800020204     C                   ENDIF
159900020204     C                   ENDIF
160000020204     C*
160100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160200020204     C                   Z-ADD     vabVMD        wrkDec
160300020204     C                   IF        wrkDec > *zeros
160400020204     C                   IF        vabVAD = 'ITL'
160500020204     C                   EVAL      vabVMD = *zeros
160600020204     C                   ENDIF
160700020204     C                   ENDIF
160800020204     C*
160900020204     C                   ENDSR
161000020204     C***
161100080617
161200080617
161300080617
161400080617     C*----------------------------------------------------*
161500080617     C*  CONTROLLO NUMERICITA' CAMPI
161600080617     C*----------------------------------------------------*
161700080617     C     CHKNUM        BEGSR
161800080617     C*
161900080617     C                   IF        PiDecChr = *blanks
162000080617     C                   EVAL      PiDecChr = CharNUM
162100080617     C                   ENDIF
162200080617     C*
162300080617     C                   callp     UBISNUM_Check(PiStr
162400080617     C                                          :PiDecChr
162500080617     C                                          :PiVal
162600080617     C                                          :PiNum
162700080617     C                                          :PiInt)
162800080617     C*
162900080617     C                   ENDSR
163000080617     C***
163100010330
163200010601
163300010601
163400010601
163500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163600010601     C     repfil        BEGSR
163700010601     C*
163800010601     C                   if        invfil = *zeros and
163900010601     C                             depfil > *zeros and
164000010629     C                             (vinflg = *blanks or
164100010629     C                              vinflg = *zeros)
164200010601     C
164300010601     C                   eval      invfil = depfil
164400010601     C                   endif
164500010601     C*
164600010601     C                   if        depfil <> invfil and
164700010601     C                             invfil > *zeros
164800010601     C                   eval      flgMulti = '1'
164900010601     C                   if        vinflg = *blanks
165000010601     C                   add       1             cntNonEl
165100010601     C                   endif
165200010601     C                   endif
165300010601     C*
165400010601     C                   if        vinflg = '2'
165500010601     C                   eval      flgStato = '2'
165600010601     C                   endif
165700010601     C*
165800010601     C                   ENDSR
165900010601     C***
166000010601
166100010601
166200010601
166300010330
166400040119      /TITLE Invio dei dati al punto operativo.
166500040119     C     invio         BEGSR
166600040119     C*
166700040119     C* 1° invio FIVAT
166800040119     C                   reset                   dscmz
166900040119     C                   move      vlrpoi        cmzdst
167000040119     C                   eval      cmzfld = 'FIVATWWR'
167100040119     C                   eval      cmzmbd = vlrhdl
167200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167300040119     C***                if        prmfir = *blanks
167400040119     C                   eval      cmzfla = 'FIVAT00F'
167500040119     C                   eval      cmzmba = 'FIVAT00F'
167600040119     C***                else
167700040119     C***                eval      cmzfla = prmfir
167800040119     C***                eval      cmzmba = prmfir
167900040119     C***                endif
168000040119     C                   eval      cmznrr = *zeros
168100040119     C                   move      §ctrokvt      cmznrr
168200040119     C                   eval      cmzlba = vlrfl1
168300040119     C                   call(e)   'TIS711C'
168400040119     C                   parm                    dscmz
168500040119     C                   parm      *blanks       esito
168600040119     C                   if        %error
168700040119     C                             or cmzerr = '1'
168800040119     C                             or esito  = '1'
168900040119     C                   eval      wrkesito = '3'
169000040119     C                   else
169100040119     C*
169200040119     C* 2° invio FIVAB
169300040119     C                   reset                   dscmz
169400040119     C                   move      vlrpoi        cmzdst
169500040119     C                   eval      cmzfld = vlrfou
169600040119     C                   eval      cmzmbd = vlrhdl
169700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169800040119     C***                if        prmfir = *blanks
169900040119     C                   eval      cmzfla = 'FIVAB00F'
170000040119     C                   eval      cmzmba = 'FIVAB00F'
170100040119     C***                else
170200040119     C***                eval      cmzfla = prmfir
170300040119     C***                eval      cmzmba = prmfir
170400040119     C***                endif
170500040119     C                   eval      cmznrr = *zeros
170600040119     C                   move      §ctrokvb      cmznrr
170700040119     C                   eval      cmzlba = vlrfl1
170800040119     C                   call(e)   'TIS711C'
170900040119     C                   parm                    dscmz
171000040119     C                   parm      *blanks       esito
171100040119     C                   if        %error
171200040119     C                             or cmzerr = '1'
171300040119     C                             or esito  = '1'
171400040119     C                   eval      wrkesito = '3'
171500040119     C                   endif
171600040119     C                   endif
171700040119     C*
171800040119     C                   ENDSR
171900040119     C***
172000010601
172100010601
172200010601
172300010601
172400010601      /TITLE Invio dei dati al punto operativo.
172500010601     C     opeini        BEGSR
172600010601     C*
172700010601     C* Inizializzo flag e contatori operativi
172800010601     C                   movel     '0'           flgGiro           1
172900010601     C                   movel     '0'           flgMulti          1
173000010601     C                   movel     '1'           flgStato          1
173100010615     C                   movel     '0'           flgOk             1
173200010601     C                   z-add     *zeros        cntNonEl         10 0
173300010601     C                   z-add     *zeros        depfil            3 0
173400010601     C                   z-add     *zeros        invfil            3 0
173500010601     C*
173600010601     C                   ENDSR
173700010601     C***
173800070326
173900070326
174000070326
174100070326
174200080916     C     *pssr         BEGSR
174300070329     C*
174400080916     C                   if        %open(tivin00r)
174500080916     C                   close     tivin00r
174600080916     C                   endif
174700080916     C                   if        %open(fivabwwr)
174800080916     C                   close     fivabwwr
174900080916     C                   endif
175000080916     C                   if        %open(fivatwwr)
175100080916     C                   close     fivatwwr
175200080916     C                   endif
175300070326     C*
175400070326     C* Effettuo la chiamata al CLLE preposto
175500080916     C                   call(e)   'TITVVTC'
175600080916     C                   parm                    parccm
175700080916     C                   parm                    parmbr
175800080916     C                   parm      '2'           paropz
175900070326     C*
176000080916     C                   eval      wrkesito = '2'
176100070404     C*
176200080916     C                   seton                                        LR
176300070326     C*
176400080916     C                   ENDSR     '*CANCL'
176500070326     C***
176600070326
176700070326
176800010330
176900010330
177000000613     C     *inzsr        BEGSR
177100990910     C*
177200990910     C     *entry        plist
177300990920     C                   parm                    tivlrds
177400990921     C                   parm      wrkesito      esito
177500000724     C                   parm                    prmlit
177600000710     C                   parm                    prmfir
177700010330     C*
177800010330     C* CALCOLA LA DATA CORRENTE
177900010330     C                   time                    wn14             14 0
178000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
178100010330     C                   z-add     wn8           g08dat
178200010330     C                   z-add     *zeros        g08inv
178300010330     C                   movel     '0'           g08err
178400010330     C                   call      'XSRDA8'
178500010330     C                   parm                    wlbda8
178600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
178700000613     C*
178800000613     C                   ENDSR
178900000613     C***
179000990908
179100090220     Otitv1n0p  E            riepilogo         2
179200990915     O                                              'Upload via Internet'
179300990915     O                                           +1 'Traduzione TIVIN00R -'
179400021025     O                                           +1 'FIVABWWR'
179500990915     O                                           +1 'Testate bolle'
179600990915     O          E            riepilogo   2
179700990915     O                       wrkdata
179800990915     O                       wrkora              +1
179900990915     O                       procname            +1
180000990915     O          E            riepilogo   2
180100990915     O                                              'Cliente..................:'
180200010605     O                       VABCCM        z     +1
180300990915     O          E            riepilogo   2
180400990920     O                                              'Riferimento Strategi.....:'
180500990920     O                       vlrhdl              +1
180600990915     O          E            riepilogo   2
180700990915     O                                              'Giusti...................:'
180800040119     O                       §CTROKVB      2   +  1
180900990915     O          E            riepilogo   2
181000990915     O                                              'Sbagliati e corretti.....:'
181100971022     O                       §CTRMO        2   +  1
181200990915     O          E            riepilogo   2
181300990915     O                                              'Sbagliati e scartati.....:'
181400971022     O                       §CTRNO        2   +  1
181500000613
181600090220     Otitv1n0ps E            testdett          2
181700000613     O                                              'Upload via Internet'
181800000613     O                                           +1 'Traduzione TIVIN00R -'
181900021025     O                                           +1 'FIVABWWR'
182000000613     O                                           +1 'Testate bolle'
182100000616     O          E            testdett    3
182200000613     O                                           +2 'N° rec'
182300000613     O                                           +3 'Anteprima contenuto'
182400000616     O          E            rigadett    2
182500000613     O                       rrnum               +2
182600000621     O                       recko               +3
182700000616     O          E            findett     2
182800000613     O                       wrkdata
182900000613     O                       wrkora              +1
183000000613     O                       procname            +1
183100000616     O          E            findett     2
183200000613     O                                              'Cliente..................:'
183300010605     O                       VABCCM        z     +1
183400000616     O          E            findett     2
183500000613     O                                              'Riferimento Strategi.....:'
183600000613     O                       vlrhdl              +1
183700000616     O          E            findett     2
183800000613     O                                              'Giusti...................:'
183900040119     O                       §CTROKVB      2   +  1
184000000616     O          E            findett     2
184100000613     O                                              'Sbagliati e corretti.....:'
184200000613     O                       §CTRMO        2   +  1
184300000616     O          E            findett     2
184400000613     O                                              'Sbagliati e scartati.....:'
184500000613     O                       §CTRNO        2   +  1
184600000613** CMD - COMANDI CL
184700090220OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1N0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
184800090220OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1N0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
184900090220DLTOVR FILE(TITV1N0P TITV1N0PS) LVL(*)
185000010606
185100000613
185200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
185300090220FILL01;WRSD;VABNSP;FILL02;WIND;WCAD;WLOD;WPRD;WRSD2;WIND2;WCAD2;WLOD2;
185400090220WPRD2;VABPKB;VABNCL;VABCBO;VABNOT;VABCAS
185500080715
