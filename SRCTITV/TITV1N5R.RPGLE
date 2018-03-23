000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800090305     Ftitv1n5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100090305     Ftitv1n5ps O    f  198        PRINTER usropn
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
007700031201     D*------------------
007800031201     D* Costanti
007900031201     D*------------------
008000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008200080617     D*------------------
008300080617
008400080617     D*------------------
008500080617     D* LINKING A DEFINIZIONI ESTERNE
008600080617     D*------------------
008700080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008800090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008900010330
009000010330
009100010330
009200990915     C                   time                    wrkdata
009300990915     C                   time                    wrkora
009400000913     C                   reset                   rrnum
009500990921     C                   reset                   esito
009600081014     C                   reset                   wrkesito
009700010601     C*
009800010601     C                   exsr      opeini
009900010605     C                   exsr      rwvab
010000081014     C*
010100081014     C                   exsr      endela
010200010601     C*
010300010601     C                   seton                                        lr
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010900010601     C*--------------------------------------------------------
011000010601     C     PREELA        BEGSR
011100010601     C*
011200010601     C* SE OCCORRE SPEDIRE IN FILIALE
011300010601     C                   if        invfil <> *zeros and
011400010601     C                             flgGiro = '0'
011500010601     C*
011600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010601     C                   eval      flgGiro = '1'
011800000724     C*
011900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012100010601     C     invfil        chain     azorg01l
012200000724     C                   if        %found
012300000724     C                   movel(p)  CMD(1)        depcmd
012400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012500000724     C*
012600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012700011113     C                   Z-ADD     150           LENGH            15 5
012800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012900000724     C                   PARM                    depcmd
013000000724     C                   PARM                    LENGH
013100000724     C*
013200000724     C                   endif
013300000616     C*
013400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013500000616     C                   movel(p)  CMD(2)        depcmd
013600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013700000616     C*
013800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013900011113     C                   Z-ADD     150           LENGH            15 5
014000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014100000616     C                   PARM                    depcmd
014200000616     C                   PARM                    LENGH
014300000613     C*
014400090305     C                   if        not %open(titv1n5ps)
014500090305     C                   open      titv1n5ps
014600070329     C                   except    testdett
014700000613     C                   endif
014800010601     C*
014900010601     C                   endif
015000010601     C*
015100010601     C                   ENDSR
015200010601     C***
015300010601
015400010601
015500010601
015600010601
015700010601     C*--------------------------------------------------------
015800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015900010601     C*--------------------------------------------------------
016000010601     C     ENDELA        BEGSR
016100081014     C*
016200081014     C                   if        %open(tivin00r)
016300081014     C                   close     tivin00r
016400081014     C                   endif
016500081014     C                   if        %open(fivabwwr)
016600081014     C                   close     fivabwwr
016700081014     C                   endif
016800081014     C                   if        %open(fivatwwr)
016900081014     C                   close     fivatwwr
017000081014     C                   endif
017100040119     C*
017200040119     C* Effettuo la chiamata al CLLE preposto
017300040119     C                   call(e)   'TITVVTC'
017400040510     C                   parm                    parccm
017500040119     C                   parm                    parmbr
017600040119     C                   parm      '2'           paropz
017700050201     C*
017800050201     C* Effettuo lancio TISI95 solo x chiusura
017900050201     C                   CLEAR                   TISI95DS
018000050201     C                   EVAL      I95TLA = 'C'
018100050201     C                   CALL      'TISI95R'
018200050201     C                   PARM                    TISI95DS
018300000613     C*
018400090305     C                   if        %open(titv1n5ps)
018500000616     C                   except    findett
018600090305     C                   close     titv1n5ps
018700000613     C                   endif
018800000616     C*
018900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019000011113     C                   Z-ADD     150           LENGH            15 5
019100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019200000616     C                   PARM                    CMD(3)
019300000616     C                   PARM                    LENGH
019400000616     C*
019500010601     C                   ENDSR
019600010601     C***
019700010601
019800010601
019900010601
020000000613
020100000613     C*--------------------------------------------------------
020200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020300000613     C*--------------------------------------------------------
020400000621     C     STPR          BEGSR
020500000613     C*
020600090305     C                   if        not %open(titv1n5p)
020700090305     C   50              open      titv1n5p
020800990915     C                   endif
020900990915     C*
021000030715     C   50              except    riepilogo
021100990915     C*
021200090305     C                   if        %open(titv1n5p)
021300090305     C   50              close     titv1n5p
021400990914     C                   endif
021500000613     C*
021600000613     C                   ENDSR
021700000613     C***
021800000613
021900010601
022000010601
022100010330     C*--------------------------------------------------------
022200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022300010330     C*--------------------------------------------------------
022400010605     C     RWVAB         BEGSR
022500010330     C*
022600010330     C                   if        not %open(tivin00r)
022700010330     C                   open      tivin00r
022800010330     C                   endif
022900021025     C                   if        not %open(fivabwwr)
023000021025     C                   open      fivabwwr
023100010330     C                   endif
023200040119     C*
023300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023400040119     C                   exsr      prevat
023500040119     C*
023600040119     C                   if        chkcall = '0'
023700040119     C*
023800040119     C                   if        not %open(fivatwwr)
023900040119     C                   open      fivatwwr
024000040119     C                   endif
024100010330     C*
024200040119     C                   clear                   §CTROKVB          7 0
024300040119     C                   clear                   §CTROKVT          7 0
024400040119     C                   clear                   §CTRMO            7 0
024500040119     C                   clear                   §CTRNO            7 0
024600070823     C*
024700070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024800070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024900081016     C***                exsr      inzvar
025000081016     C***                exsr      impvab
025100010330     C*
025200030822     C                   do        *HIVAL
025300010330     C*
025400030822     C                   read      tivin00r                               70
025500010618     C*
025600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025700010618     C                   if        vinflg = '1'
025800010618     C                   eval      flgOk = '1'
025900010618     C                   endif
026000040510     C*
026100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026200040510     C                   z-add     *zeros        wLenVINDTA        4 0
026300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026700040510     C                   z-add     1             wFlgVINDTA
026800040510     C                   leave
026900040510     C                   endif
027000040510     C                   enddo
027100010618     C*
027200040510     C                   if        vindta > *blanks AND
027300040510     C                             wFlgVINDTA = 1
027400010330     C                   add       1             rrnum
027500010330     C*
027600010601     C                   if        *in70 = *off and
027700010330     C                             (vinflg = *blanks
027800010330     C                              or vinflg = '0'
027900010330     C                              or vinflg = '2')
028000010330     C*
028100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028200010711     C                   if        vinflg = *blanks or vinflg = '0'
028300010711     C                   clear                   vinmsg
028400010711     C                   endif
028500010601     C*
028600070925     C* Aggiungo un byte a blanks all'inizio d ogni record
028700070925     C                   eval      vindta  = ' ' + %trim(vindta)
028800070925     C*
028900010330     C                   exsr      inzvar
029000010605     C                   exsr      impvab
029100010601     C*
029200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029300010605     C                   eval      depfil = VABLNP
029400010601     C                   exsr      repfil
029500010601     C                   if        depfil = invfil
029600021025     C                   if        vlrpoi = 999
029700030822     C                   move(P)   invfil        VABFGS
029800021025     C                   else
029900030822     C                   move(P)   vlrpoi        VABFGS
030000021025     C                   endif
030100010601     C*
030200010601     C                   exsr      PREELA
030300010601     C*
030400010604     C* Ebbene...
030500010604     C*
030600040119     C  N31              add       1             §CTROKVB
030700040119     C   32              add       1             §CTRMO
030800040119     C   31              add       1             §CTRNO
030900030822     C                   if        wGiro = 2
031000040510     C* Scrivo sempre il VAT
031100040510     C  N31              exsr      wrivat
031200040510     C* Scrivo il VAB solo a rottura d spedizione
031300040119     C                   if        depspe =  *zeros OR
031400040119     C                             depspe <> vabNSP
031500040119     C  N31              exsr      wrivab
031600040119     C  N31              eval      depspe = vabNSP
031700040119     C                   endif
031800040119     C                   endif
031900010604     C*
032000010604     C                   if        *in31 = *off and
032100010604     C                             *in32 = *off
032200010604     C                   eval      vinflg = '1'
032300010604     C                   else
032400010604     C                   eval      recko = vindta
032500010604     C                   except    rigadett
032600010604     C                   eval      vinflg = '2'
032700010604     C                   endif
032800010604     C                   endif
032900010601     C*
033000010604     C                   endif
033100010604     C*
033200010330     C                   else
033300010330     C                   eval      vinflg = '1'
033400010330     C                   endif
033500010601     C*
033600010601     C  N70              update    tivin000
033700010330     C*
033800030822     C  N70              enddo
033900040119     C*
034000040119     C                   endif                                                  (endif - chkcall)
034100010601     C*
034200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034300010601     C                   if        cntNonEl = *zeros or
034400010601     C                             flgMulti = '0'
034500010330     C* Se non ci sono record con errori ...
034600010601     C                   if        §ctrno = 0 and
034700010604     C                             §ctrmo = 0 and
034800010601     C                             flgStato <> '2'
034900010330     C* ... restituisco esito OK.
035000010330     C                   eval      wrkesito = '0'
035100010330     C                   else
035200040119     C                   if        §ctrokvb > 0
035300010330     C                   eval      wrkesito = '1'
035400010330     C                   else
035500010615     C                   if        flgOk = '0'
035600010615     C                   eval      wrkesito = '2'
035700010615     C                   else
035800010615     C                   eval      wrkesito = '6'
035900010615     C                   endif
036000010330     C                   endif
036100010330     C                   endif
036200010601     C                   else
036300010601     C                   eval      wrkesito = '9'
036400010601     C                   endif
036500010330     C*
036600010330     C                   if        %open(tivin00r)
036700010330     C                   close     tivin00r
036800010330     C                   endif
036900021025     C                   if        %open(fivabwwr)
037000021025     C                   close     fivabwwr
037100010330     C                   endif
037200040512     C                   if        %open(fivatwwr)
037300040512     C                   close     fivatwwr
037400040512     C                   endif
037500010601     C*
037600010601     C                   if        vlrpoi <> 999
037700010601     C                   eval      invfil = vlrpoi
037800010601     C                   endif
037900010330     C*
038000040119     C                   if        §ctrokvb > 0
038100010601     C                             and invfil > *zeros
038200010330     C                   exsr      invio
038300010330     C                   endif
038400010601     C*
038500010618     C                   if        flgGiro = '1'
038600081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
038700010618     C                   endif
038800010330     C*
038900010330     C                   ENDSR
039000010330     C***
039100040119
039200040119
039300040119
039400040119     C*----------------------------------------------------*
039500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039600040119     C*----------------------------------------------------*
039700040510     C     WRIVAB        BEGSR
039800040119     C*
039900060331     C* Gestisco l'eventuale rottura x numero spedizione
040000070823     C                   if        VABNSP <> depspe
040100070301     C                   if        wVATNOT_E <> *blanks
040200080125     C                   if        VABCTM = *blanks
040300080125     C                   eval      VABCTM = '7Q'
040400080125     C                   endif
040500070301     C                   endif
040600040119     C                   write     fivab000                                     => scarico il VAB
040700060331     C                   endif
040800040119     C*
040900040119     C                   ENDSR
041000040119
041100040119
041200040119
041300040119     C*----------------------------------------------------*
041400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041500040119     C*----------------------------------------------------*
041600040510     C     WRIVAT        BEGSR
041700060512     C*
041800060512     C* Inizializzo i campi chiave
041900060512     C                   eval      VATFGS = VABFGS
042000060512     C                   eval      VATAAS = VABAAS
042100060512     C                   eval      VATCCM = VABCCM
042200070823     C                   eval      VATNRS = VABNRS
042300060512     C                   eval      VATNSP = VABNSP
042400060512     C                   eval      VATLNP = VABLNP
042500060331     C*
042600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042700060331     C                   if        wVATNOT_A <> *blanks
042800060331     C                   eval      VATNOT = wVATNOT_A
042900060331     C                   eval      VATTRC = 'A'
043000060331     C                   write     FIVAT000
043100060331     C                   add       1             §CTROKVT
043200060331     C                   endif
043300060331     C*
043400060331     C                   if        wVATNOT_B <> *blanks
043500060331     C                   eval      VATNOT = wVATNOT_B
043600060331     C                   eval      VATTRC = 'B'
043700060331     C                   write     FIVAT000
043800060331     C                   add       1             §CTROKVT
043900060331     C                   endif
044000060331     C*
044100080625     C                   if        wVATNOT_E <> *blanks
044200060331     C                   eval      VATNOT = wVATNOT_E
044300060331     C                   eval      VATTRC = 'E'
044400080716     C                   write     FIVAT000
044500080716     C                   add       1             §CTROKVT
044600080625     C                   endif
044700040119     C*
044800040119     C                   ENDSR
044900040119
045000040119
045100040119
045200040119     C*----------------------------------------------------*
045300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045400040119     C*----------------------------------------------------*
045500040119     C     PREVAT        BEGSR
045600040119     C*
045700040119     C* Compongo il nome del membro da dare al FIVATWWR
045800040119     C                   eval      parmbr = vlrhdl
045900040119     C                   movel     'M'           parmbr
046000040510     C                   eval      parccm = vlrksc
046100040119     C                   eval      paropz = '1'
046200040119     C* Effettuo la chiamata al CLLE preposto
046300040119     C                   call(e)   'TITVVTC'
046400040119     C                   parm                    parccm
046500040119     C                   parm                    parmbr
046600040119     C                   parm                    paropz
046700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046800040119     C                   if        %error
046900040119     C                   movel     '1'           chkcall
047000040119     C                   else
047100040119     C                   movel     '0'           chkcall
047200040119     C                   endif
047300040119     C*
047400040119     C                   ENDSR
047500010601
047600010601
047700010601
047800010330     C*----------------------------------------------------*
047900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048000010330     C*----------------------------------------------------*
048100040119     C     INZVAR        BEGSR
048200010330     C*
048300030822     C* Inizializzo variabili di wrk
048400020204     C                   Z-ADD     *zeros        Num5_0            5 0
048500060331     C                   MOVEL     *blanks       wVATNOT_A
048600060331     C                   MOVEL     *blanks       wVATNOT_B
048700060331     C                   MOVEL     *blanks       wVATNOT_E
048800090220     C*
048900080617     C                   EVAL      wFlgCAS = *blanks
049000030822     C*
049100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049200030822     C                   CLEAR                   FIVAB000
049300040119     C                   CLEAR                   FIVAT000
049400030822     C                   CLEAR                   SkSplitCSV
049500030822     C                   EVAL      i = 1
049600030822     C                   EVAL      posDa = *zeros
049700030822     C                   EVAL      posA  = *zeros
049800030822     C*
049900030822     C* Reimposto i valori di default
050000030822     C                   EXSR      DEFCAM
050100030822     C*
050200010330     C                   ENDSR
050300010330     C*----------------------------------------------------*
050400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050500010330     C*----------------------------------------------------*
050600010330     C     DEFCAM        BEGSR
050700030822     C*
050800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050900030715     C* e delimitatore testo.
051000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051300030822     C*
051400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
051600030715     C                   SETON                                        50
051700030715     C                   ELSE
051800030715     C                   SETOFF                                       50
051900030715     C                   ENDIF
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
061900030822     C                   Z-ADD     PiVal         VABPKB
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
079400030515     C*
079500010607     C                   MOVEL     datcor        VABAAS
079600010607     C                   MOVE      datcor        VABMGS
079700010607     C*
079800030715     C**********
079900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080000030715     C**********
080100030715     C                   IF        wGiro = *zeros
080200080716     C*
080300080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080400080716     C                   EVAL      depvinDTA = vinDTA
080500040113     C*
080600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080800040510     C                             %trim(intcol(3))<>*blanks
080900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081000040113     C                                      %trim(intcol(3))
081100070823     C     ';':CharCSV   XLATE     vindta        vindta
081200040510     C                   ENDIF
081300080626     C*
081400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081500031201     C     minu:maiu     XLATE     vindta        vindta
081600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081700030715     C                   DOW       posDa <= %len(%trim(vindta))
081800030715     C*
081900030715     C* Gestisco il 1° campo
082000030715     C                   IF        i = 1
082100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082200030715     C                   EVAL      posDa = 1
082300030715     C* Eseguo lo scan x trovare la fine del primo campo
082400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082500030715     C* A questo punto "estraggo" il campo corrente
082600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082700030715     C                                                    (posA-posDa))
082800030715     C* X i campi successivi al 1°
082900030715     C                   ELSE
083000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083100030715     C                   EVAL      posDa = posA + 1
083200030715     C* Eseguo lo scan x trovare la fine del campo corrente
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* Gestisco l'ultimo campo
083500030715     C                   IF        posA = *zeros
083600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   LEAVE
084100030715     C                   ELSE
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   ENDIF
084600030715     C                   ENDIF
084700030715     C* Incremento il contatore d campo
084800030715     C                   EVAL      i = i +1
084900030715     C                   ENDDO
085000030715     C*
085100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085200030715     C* delimitazione testo
085300030715     C                   EVAL      i = 1
085400030715     C                   DOW       i < %elem(SkSplitFLD)
085500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085700030715     C                   EVAL      i = i + 1
085800030715     C                   ENDDO
085900030715     C                   EVAL      wGiro = 1
086000080716     C* Ripristino la 1° riga originale
086100080716     C                   EVAL      vinDTA = depvinDTA
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
087800031204     C                   ENDDO
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
093200030715     C*
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
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiNum=*on
113900030721     C                   Z-ADD     PiVal         VABPKB
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200030721     C                   EVAL      VABPKB = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABPKB'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** CAS
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114900080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
115000080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
115100081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
115200081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
115300080125     C                   EVAL      wFlgCAS = '1'
115400080125     C                   ENDIF
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiNum=*on
115800030721     C                   Z-ADD     PiVal         VABCAS
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      VABCAS = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABCAS'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** CCM
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on      AND
117100030721     C                             PiVal<=9999999 AND
117200030721     C                             PiVal>*zeros
117300030721     C                   Z-ADD     PiVal         VABCCM
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABCCM = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800040510     C                             + ' ' + 'VABCCM'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** LNP
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on    AND
118600030721     C                             PiVal<=999   AND
118700030721     C                             PiVal>*zeros
118800030721     C                   Z-ADD     PiVal         VABLNP
118900030721     C                   ELSE
119000040715     C                   SETON                                        32
119100030721     C                   EVAL      VABLNP = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300040510     C                             + ' ' + 'VABLNP'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** NRS
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on AND
120100030721     C                             PiVal<=99
120200030721     C                   Z-ADD     PiVal         VABNRS
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABNRS'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** LNA
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on    AND
121400030721     C                             PiVal<=999   AND
121500030721     C                             PiVal>*zeros
121600030721     C                   Z-ADD     PiVal         VABLNA
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      VABLNA = *zeros
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABLNA'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** CTR
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on  AND
122900030721     C                             PiVal<=999
123000030721     C                   Z-ADD     PiVal         VABCTR
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABCTR'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** IAS
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiNum=*on
124200030721     C                   Z-ADD     PiVal         VABIAS
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABIAS'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** VLB
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiNum=*on
125400030721     C                   Z-ADD     PiVal         VABVLB
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABVLB'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** QFT
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiNum=*on
126600030721     C                   Z-ADD     PiVal         VABQFT
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABQFT'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** NCD
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on      AND
127800030721     C                             PiVal<=9999999
127900030721     C                   Z-ADD     PiVal         VABNCD
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABNCD'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** NCA
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on      AND
129100030721     C                             PiVal<=9999999
129200030721     C                   Z-ADD     PiVal         VABNCA
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABNCA'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** ZNC
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on AND
130400030721     C                             PiVal<=99
130500030721     C                   Z-ADD     PiVal         VABZNC
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABZNC'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** DCR
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiInt=*on       AND
131700030721     C                             PiVal<=99999999
131800030721     C                   Z-ADD     PiVal         VABDCR
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABDCR'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** HCR
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on   AND
133000030721     C                             PiVal<=9999
133100030721     C                   Z-ADD     PiVal         VABHCR
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABHCR'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** VMD
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiNum=*on
134300030721     C                   Z-ADD     PiVal         VABVMD
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134700030721     C                             + ' ' + 'VABVMD'
134800030721     C                   ENDIF
134900030721     C                   ENDIF
135000030721     C*** ANT
135100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135200030721     C                   EVAL      PiStr=SkSplitCSV(i)
135300030721     C                   EXSR      CHKNUM
135400030721     C                   IF        PiInt=*on        AND
135500030721     C                             PiVal<=999999999
135600030721     C                   Z-ADD     PiVal         VABANT
135700030721     C                   ELSE
135800030721     C                   SETON                                        32
135900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136000030721     C                             + ' ' + 'VABANT'
136100030721     C                   ENDIF
136200030721     C                   ENDIF
136300030721     C*** CMO
136400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136500030721     C                   EVAL      PiStr=SkSplitCSV(i)
136600030721     C                   EXSR      CHKNUM
136700030721     C                   IF        PiInt=*on
136800030721     C                   Z-ADD     PiVal         Num5_0
136900030721     C                   MOVEL(p)  Num5_0        VABCMO
137000030721     C                   ELSE
137100030721     C                   SETON                                        32
137200030721     C                   EVAL      VABCMO = *zeros
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABCMO'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700040510     C*
137800040510     C* Reperisco i campi del FIVAT
137900040510     C*** VATNOT
138000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138200040510     C                   ENDIF
138300040510     C*** VATTRC
138400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138600040510     C                   ENDIF
138700060331     C*** VATNOT_A
138800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139000060331     C                   ENDIF
139100060331     C*** VATNOT_B
139200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139400060331     C                   ENDIF
139500060331     C*** VATNOT_E
139600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139800060331     C                   ENDIF
139900070823     C*
140000071029     C***  ===>  Gestione campi "particolari" dentro ciclo
140100090220     C*
140200090305     C*** W_VABTSP
140300090305     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTSP'
140400090305     C                   IF        %trim(SkSplitCSV(i)) = 'X'
140500090305     C                   EVAL      VABTSP = 'E'
140600090305     C                   ENDIF
140700090220     C                   ENDIF
140800090305     C*
140900090305     C*** W_VABTC1
141000090305     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTC1'
141100090305     C                   IF        %trim(SkSplitCSV(i)) = 'X'
141200090305     C                   EVAL      VABTC1 = 'A'
141300090305     C                   ENDIF
141400090305     C                   ENDIF
141500070925     C*
141600071029     C***  <===  -----------------------------------------
141700040510     C*
141800030715     C                   ENDIF
141900030715     C                   EVAL      i = i + 1
142000030715     C                   ENDDO
142100070925     C*
142200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142300080125     C*
142400070925     C*
142500071029     C***  <===  -----------------------------------------
142600070925     C*
142700070925     C* Considerazioni finali su CBO/CAS
142800080125     C                   IF        wFlgCAS = '1'
142900070925     C                   IF        VABCBO = '1'
143000070925     C                   EVAL      VABCBO = '4'
143100070925     C                   ENDIF
143200070925     C                   IF        VABCBO = '2'
143300070925     C                   EVAL      VABCBO = '6'
143400070925     C                   ENDIF
143500070925     C                   ENDIF
143600070925     C***  <===  ----------------------------
143700040510     C*
143800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
143900040510     C                   IF        VABNSP = *zeros
144000040510     C                   SETON                                        31
144100040510     C                   ENDIF
144200040715     C*
144300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
144400040715     C                   IF        VABLNP = *zeros
144500040715     C                   SETON                                        31
144600040715     C                   ENDIF
144700040510     C*
144800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
144900040510     C                   IF        VABRMN = *zeros
145000040510     C                   Z-ADD     VABNSP        VABRMN
145100040510     C                   ENDIF
145200040510     C*
145300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
145400040510     C                   IF        VABRMA = *blanks
145500040714     C                   MOVEL     VABRMN        VABRMA
145600040510     C                   ENDIF
145700040128     C*
145800040510     C* Se provincia nn valorizzata la reperisco
145900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146000040510     C                   IF        VABPRD = *blanks
146100040128     C                   CLEAR                   TISI95DS
146200040128     C                   EVAL      I95TCN = '3'
146300040128     C                   Z-ADD     datcor        I95DAT
146400040128     C                   EVAL      I95NAR = VABNZD
146500040128     C                   EVAL      I95CAP = VABCAD
146600040128     C                   EVAL      I95LOC = VABLOD
146700040128     C                   CALL      'TISI95R'
146800040128     C                   PARM                    TISI95DS
146900040128     C                   EVAL      VABPRD = O95PRV
147000040510     C                   ENDIF
147100020204     C*
147200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
147300020204     C                   EXSR      CHKIMPDIV
147400030715     C                   ENDIF
147500020204     C*
147600010607     C                   ENDSR
147700010607     C*----------------------------------------------------*
147800010601
147900020204
148000020204     C*----------------------------------------------------*
148100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
148200020204     C*----------------------------------------------------*
148300020204     C     CHKIMPDIV     BEGSR
148400020204     C*
148500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
148600020204     C                   Z-ADD     *zeros        wrkDec            9 9
148700020204     C*
148800020204     C* Come prima cosa effettuo considerazioni sulla divisa
148900020204     C                   IF        vabIAS > *zeros
149000020204     C                   IF        vabVAS <> 'EUR'
149100020204     C                   EVAL      vabVAS =  'ITL'
149200020204     C                   ENDIF
149300020204     C                   ENDIF
149400020204     C*
149500020204     C                   IF        vabCAS > *zeros
149600020204     C                   IF        vabVCA <> 'EUR'
149700020204     C                   EVAL      vabVCA =  'ITL'
149800020204     C                   ENDIF
149900020204     C                   ENDIF
150000020204     C*
150100020204     C                   IF        vabVMD > *zeros
150200020204     C                   IF        vabVAD <> 'EUR'
150300020204     C                   EVAL      vabVAD =  'ITL'
150400020204     C                   ENDIF
150500020204     C                   ENDIF
150600020204     C*
150700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
150800020204     C                   Z-ADD     vabIAS        wrkDec
150900020204     C                   IF        wrkDec > *zeros
151000020204     C                   IF        vabVAS = 'ITL'
151100020204     C                   EVAL      vabIAS = *zeros
151200020204     C                   ENDIF
151300020204     C                   ENDIF
151400020204     C*
151500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
151600020204     C                   Z-ADD     vabCAS        wrkDec
151700020204     C                   IF        wrkDec > *zeros
151800020204     C                   IF        vabVCA = 'ITL'
151900020204     C                   EVAL      vabCAS = *zeros
152000020204     C                   ENDIF
152100020204     C                   ENDIF
152200020204     C*
152300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
152400020204     C                   Z-ADD     vabVMD        wrkDec
152500020204     C                   IF        wrkDec > *zeros
152600020204     C                   IF        vabVAD = 'ITL'
152700020204     C                   EVAL      vabVMD = *zeros
152800020204     C                   ENDIF
152900020204     C                   ENDIF
153000020204     C*
153100020204     C                   ENDSR
153200020204     C***
153300080617
153400080617
153500080617
153600080617     C*----------------------------------------------------*
153700080617     C*  CONTROLLO NUMERICITA' CAMPI
153800080617     C*----------------------------------------------------*
153900080617     C     CHKNUM        BEGSR
154000080617     C*
154100080617     C                   IF        PiDecChr = *blanks
154200080617     C                   EVAL      PiDecChr = CharNUM
154300080617     C                   ENDIF
154400080617     C*
154500080617     C                   callp     UBISNUM_Check(PiStr
154600080617     C                                          :PiDecChr
154700080617     C                                          :PiVal
154800080617     C                                          :PiNum
154900080617     C                                          :PiInt)
155000080617     C*
155100080617     C                   ENDSR
155200080617     C***
155300010330
155400010601
155500010601
155600010601
155700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
155800010601     C     repfil        BEGSR
155900010601     C*
156000010601     C                   if        invfil = *zeros and
156100010601     C                             depfil > *zeros and
156200010629     C                             (vinflg = *blanks or
156300010629     C                              vinflg = *zeros)
156400010601     C
156500010601     C                   eval      invfil = depfil
156600010601     C                   endif
156700010601     C*
156800010601     C                   if        depfil <> invfil and
156900010601     C                             invfil > *zeros
157000010601     C                   eval      flgMulti = '1'
157100010601     C                   if        vinflg = *blanks
157200010601     C                   add       1             cntNonEl
157300010601     C                   endif
157400010601     C                   endif
157500010601     C*
157600010601     C                   if        vinflg = '2'
157700010601     C                   eval      flgStato = '2'
157800010601     C                   endif
157900010601     C*
158000010601     C                   ENDSR
158100010601     C***
158200010601
158300010601
158400010601
158500010330
158600040119      /TITLE Invio dei dati al punto operativo.
158700040119     C     invio         BEGSR
158800040119     C*
158900040119     C* 1° invio FIVAT
159000040119     C                   reset                   dscmz
159100040119     C                   move      vlrpoi        cmzdst
159200040119     C                   eval      cmzfld = 'FIVATWWR'
159300040119     C                   eval      cmzmbd = vlrhdl
159400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159500040119     C***                if        prmfir = *blanks
159600040119     C                   eval      cmzfla = 'FIVAT00F'
159700040119     C                   eval      cmzmba = 'FIVAT00F'
159800040119     C***                else
159900040119     C***                eval      cmzfla = prmfir
160000040119     C***                eval      cmzmba = prmfir
160100040119     C***                endif
160200040119     C                   eval      cmznrr = *zeros
160300040119     C                   move      §ctrokvt      cmznrr
160400040119     C                   eval      cmzlba = vlrfl1
160500040119     C                   call(e)   'TIS711C'
160600040119     C                   parm                    dscmz
160700040119     C                   parm      *blanks       esito
160800040119     C                   if        %error
160900040119     C                             or cmzerr = '1'
161000040119     C                             or esito  = '1'
161100040119     C                   eval      wrkesito = '3'
161200040119     C                   else
161300040119     C*
161400040119     C* 2° invio FIVAB
161500040119     C                   reset                   dscmz
161600040119     C                   move      vlrpoi        cmzdst
161700040119     C                   eval      cmzfld = vlrfou
161800040119     C                   eval      cmzmbd = vlrhdl
161900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162000040119     C***                if        prmfir = *blanks
162100040119     C                   eval      cmzfla = 'FIVAB00F'
162200040119     C                   eval      cmzmba = 'FIVAB00F'
162300040119     C***                else
162400040119     C***                eval      cmzfla = prmfir
162500040119     C***                eval      cmzmba = prmfir
162600040119     C***                endif
162700040119     C                   eval      cmznrr = *zeros
162800040119     C                   move      §ctrokvb      cmznrr
162900040119     C                   eval      cmzlba = vlrfl1
163000040119     C                   call(e)   'TIS711C'
163100040119     C                   parm                    dscmz
163200040119     C                   parm      *blanks       esito
163300040119     C                   if        %error
163400040119     C                             or cmzerr = '1'
163500040119     C                             or esito  = '1'
163600040119     C                   eval      wrkesito = '3'
163700040119     C                   endif
163800040119     C                   endif
163900040119     C*
164000040119     C                   ENDSR
164100040119     C***
164200010601
164300010601
164400010601
164500010601
164600010601      /TITLE Invio dei dati al punto operativo.
164700010601     C     opeini        BEGSR
164800010601     C*
164900010601     C* Inizializzo flag e contatori operativi
165000010601     C                   movel     '0'           flgGiro           1
165100010601     C                   movel     '0'           flgMulti          1
165200010601     C                   movel     '1'           flgStato          1
165300010615     C                   movel     '0'           flgOk             1
165400010601     C                   z-add     *zeros        cntNonEl         10 0
165500010601     C                   z-add     *zeros        depfil            3 0
165600010601     C                   z-add     *zeros        invfil            3 0
165700010601     C*
165800010601     C                   ENDSR
165900010601     C***
166000070326
166100070326
166200070326
166300070326
166400080916     C     *pssr         BEGSR
166500070329     C*
166600080916     C                   if        %open(tivin00r)
166700080916     C                   close     tivin00r
166800080916     C                   endif
166900080916     C                   if        %open(fivabwwr)
167000080916     C                   close     fivabwwr
167100080916     C                   endif
167200080916     C                   if        %open(fivatwwr)
167300080916     C                   close     fivatwwr
167400080916     C                   endif
167500070326     C*
167600070326     C* Effettuo la chiamata al CLLE preposto
167700080916     C                   call(e)   'TITVVTC'
167800080916     C                   parm                    parccm
167900080916     C                   parm                    parmbr
168000080916     C                   parm      '2'           paropz
168100070326     C*
168200080916     C                   eval      wrkesito = '2'
168300070404     C*
168400080916     C                   seton                                        LR
168500070326     C*
168600080916     C                   ENDSR     '*CANCL'
168700070326     C***
168800070326
168900070326
169000010330
169100010330
169200000613     C     *inzsr        BEGSR
169300990910     C*
169400990910     C     *entry        plist
169500990920     C                   parm                    tivlrds
169600990921     C                   parm      wrkesito      esito
169700000724     C                   parm                    prmlit
169800000710     C                   parm                    prmfir
169900010330     C*
170000010330     C* CALCOLA LA DATA CORRENTE
170100010330     C                   time                    wn14             14 0
170200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
170300010330     C                   z-add     wn8           g08dat
170400010330     C                   z-add     *zeros        g08inv
170500010330     C                   movel     '0'           g08err
170600010330     C                   call      'XSRDA8'
170700010330     C                   parm                    wlbda8
170800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
170900000613     C*
171000000613     C                   ENDSR
171100000613     C***
171200990908
171300090305     Otitv1n5p  E            riepilogo         2
171400990915     O                                              'Upload via Internet'
171500990915     O                                           +1 'Traduzione TIVIN00R -'
171600021025     O                                           +1 'FIVABWWR'
171700990915     O                                           +1 'Testate bolle'
171800990915     O          E            riepilogo   2
171900990915     O                       wrkdata
172000990915     O                       wrkora              +1
172100990915     O                       procname            +1
172200990915     O          E            riepilogo   2
172300990915     O                                              'Cliente..................:'
172400010605     O                       VABCCM        z     +1
172500990915     O          E            riepilogo   2
172600990920     O                                              'Riferimento Strategi.....:'
172700990920     O                       vlrhdl              +1
172800990915     O          E            riepilogo   2
172900990915     O                                              'Giusti...................:'
173000040119     O                       §CTROKVB      2   +  1
173100990915     O          E            riepilogo   2
173200990915     O                                              'Sbagliati e corretti.....:'
173300971022     O                       §CTRMO        2   +  1
173400990915     O          E            riepilogo   2
173500990915     O                                              'Sbagliati e scartati.....:'
173600971022     O                       §CTRNO        2   +  1
173700000613
173800090305     Otitv1n5ps E            testdett          2
173900000613     O                                              'Upload via Internet'
174000000613     O                                           +1 'Traduzione TIVIN00R -'
174100021025     O                                           +1 'FIVABWWR'
174200000613     O                                           +1 'Testate bolle'
174300000616     O          E            testdett    3
174400000613     O                                           +2 'N° rec'
174500000613     O                                           +3 'Anteprima contenuto'
174600000616     O          E            rigadett    2
174700000613     O                       rrnum               +2
174800000621     O                       recko               +3
174900000616     O          E            findett     2
175000000613     O                       wrkdata
175100000613     O                       wrkora              +1
175200000613     O                       procname            +1
175300000616     O          E            findett     2
175400000613     O                                              'Cliente..................:'
175500010605     O                       VABCCM        z     +1
175600000616     O          E            findett     2
175700000613     O                                              'Riferimento Strategi.....:'
175800000613     O                       vlrhdl              +1
175900000616     O          E            findett     2
176000000613     O                                              'Giusti...................:'
176100040119     O                       §CTROKVB      2   +  1
176200000616     O          E            findett     2
176300000613     O                                              'Sbagliati e corretti.....:'
176400000613     O                       §CTRMO        2   +  1
176500000616     O          E            findett     2
176600000613     O                                              'Sbagliati e scartati.....:'
176700000613     O                       §CTRNO        2   +  1
176800000613** CMD - COMANDI CL
176900090305OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1N5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
177000090305OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1N5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
177100090305DLTOVR FILE(TITV1N5P TITV1N5PS) LVL(*)
177200010606
177300000613
177400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
177500090305FILL01;VABNSP;VABRSD;VABLOD;VABCAD;VABIND;VABNCL;VABPKB;FILL02;W_VABTSP;
177600090305W_VABTC1;VABCAS
177700080715
