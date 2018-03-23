000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800090622     Ftitv1q6p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100090622     Ftitv1q6ps O    f  198        PRINTER usropn
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
014400090622     C                   if        not %open(titv1q6ps)
014500090622     C                   open      titv1q6ps
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
018400090622     C                   if        %open(titv1q6ps)
018500000616     C                   except    findett
018600090622     C                   close     titv1q6ps
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
020600090622     C                   if        not %open(titv1q6p)
020700090622     C   50              open      titv1q6p
020800990915     C                   endif
020900990915     C*
021000030715     C   50              except    riepilogo
021100990915     C*
021200090622     C                   if        %open(titv1q6p)
021300090622     C   50              close     titv1q6p
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
024900090420     C                   exsr      inzvar
025000090420     C                   exsr      impvab
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
027100090420     C*
027200090420     C                   add       1             rrnum
027300010618     C*
027400040510     C                   if        vindta > *blanks AND
027500090420     C                             wFlgVINDTA = 1
027600010330     C*
027700010601     C                   if        *in70 = *off and
027800010330     C                             (vinflg = *blanks
027900010330     C                              or vinflg = '0'
028000010330     C                              or vinflg = '2')
028100010330     C*
028200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028300010711     C                   if        vinflg = *blanks or vinflg = '0'
028400010711     C                   clear                   vinmsg
028500010711     C                   endif
028600010601     C*
028700090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
028800090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
028900090421     C                   eval      vindta  = '*' + %trim(vindta)
029000090421     C                   endif
029100070925     C*
029200010330     C                   exsr      inzvar
029300010605     C                   exsr      impvab
029400010601     C*
029500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029600010605     C                   eval      depfil = VABLNP
029700010601     C                   exsr      repfil
029800010601     C                   if        depfil = invfil
029900021025     C                   if        vlrpoi = 999
030000030822     C                   move(P)   invfil        VABFGS
030100021025     C                   else
030200030822     C                   move(P)   vlrpoi        VABFGS
030300021025     C                   endif
030400010601     C*
030500010601     C                   exsr      PREELA
030600010601     C*
030700010604     C* Ebbene...
030800010604     C*
030900040119     C  N31              add       1             §CTROKVB
031000040119     C   32              add       1             §CTRMO
031100040119     C   31              add       1             §CTRNO
031200030822     C                   if        wGiro = 2
031300040510     C* Scrivo sempre il VAT
031400040510     C  N31              exsr      wrivat
031500040510     C* Scrivo il VAB solo a rottura d spedizione
031600040119     C                   if        depspe =  *zeros OR
031700040119     C                             depspe <> vabNSP
031800040119     C  N31              exsr      wrivab
031900040119     C  N31              eval      depspe = vabNSP
032000040119     C                   endif
032100040119     C                   endif
032200010604     C*
032300010604     C                   if        *in31 = *off and
032400010604     C                             *in32 = *off
032500010604     C                   eval      vinflg = '1'
032600010604     C                   else
032700010604     C                   eval      recko = vindta
032800010604     C                   except    rigadett
032900010604     C                   eval      vinflg = '2'
033000010604     C                   endif
033100010604     C                   endif
033200010601     C*
033300010604     C                   endif
033400010604     C*
033500010330     C                   else
033600010330     C                   eval      vinflg = '1'
033700010330     C                   endif
033800010601     C*
033900010601     C  N70              update    tivin000
034000010330     C*
034100030822     C  N70              enddo
034200040119     C*
034300040119     C                   endif                                                  (endif - chkcall)
034400010601     C*
034500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034600010601     C                   if        cntNonEl = *zeros or
034700010601     C                             flgMulti = '0'
034800010330     C* Se non ci sono record con errori ...
034900010601     C                   if        §ctrno = 0 and
035000010604     C                             §ctrmo = 0 and
035100010601     C                             flgStato <> '2'
035200010330     C* ... restituisco esito OK.
035300010330     C                   eval      wrkesito = '0'
035400010330     C                   else
035500040119     C                   if        §ctrokvb > 0
035600010330     C                   eval      wrkesito = '1'
035700010330     C                   else
035800010615     C                   if        flgOk = '0'
035900010615     C                   eval      wrkesito = '2'
036000010615     C                   else
036100010615     C                   eval      wrkesito = '6'
036200010615     C                   endif
036300010330     C                   endif
036400010330     C                   endif
036500010601     C                   else
036600010601     C                   eval      wrkesito = '9'
036700010601     C                   endif
036800010330     C*
036900010330     C                   if        %open(tivin00r)
037000010330     C                   close     tivin00r
037100010330     C                   endif
037200021025     C                   if        %open(fivabwwr)
037300021025     C                   close     fivabwwr
037400010330     C                   endif
037500040512     C                   if        %open(fivatwwr)
037600040512     C                   close     fivatwwr
037700040512     C                   endif
037800010601     C*
037900010601     C                   if        vlrpoi <> 999
038000010601     C                   eval      invfil = vlrpoi
038100010601     C                   endif
038200010330     C*
038300040119     C                   if        §ctrokvb > 0
038400010601     C                             and invfil > *zeros
038500010330     C                   exsr      invio
038600010330     C                   endif
038700010601     C*
038800010618     C                   if        flgGiro = '1'
038900081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039000010618     C                   endif
039100010330     C*
039200010330     C                   ENDSR
039300010330     C***
039400040119
039500040119
039600040119
039700040119     C*----------------------------------------------------*
039800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039900040119     C*----------------------------------------------------*
040000040510     C     WRIVAB        BEGSR
040100040119     C*
040200060331     C* Gestisco l'eventuale rottura x numero spedizione
040300070823     C                   if        VABNSP <> depspe
040400070301     C                   if        wVATNOT_E <> *blanks
040500080125     C                   if        VABCTM = *blanks
040600080125     C                   eval      VABCTM = '7Q'
040700080125     C                   endif
040800070301     C                   endif
040900040119     C                   write     fivab000                                     => scarico il VAB
041000060331     C                   endif
041100040119     C*
041200040119     C                   ENDSR
041300040119
041400040119
041500040119
041600040119     C*----------------------------------------------------*
041700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041800040119     C*----------------------------------------------------*
041900040510     C     WRIVAT        BEGSR
042000060512     C*
042100060512     C* Inizializzo i campi chiave
042200060512     C                   eval      VATFGS = VABFGS
042300060512     C                   eval      VATAAS = VABAAS
042400060512     C                   eval      VATCCM = VABCCM
042500070823     C                   eval      VATNRS = VABNRS
042600060512     C                   eval      VATNSP = VABNSP
042700060512     C                   eval      VATLNP = VABLNP
042800060331     C*
042900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043000060331     C                   if        wVATNOT_A <> *blanks
043100060331     C                   eval      VATNOT = wVATNOT_A
043200060331     C                   eval      VATTRC = 'A'
043300060331     C                   write     FIVAT000
043400060331     C                   add       1             §CTROKVT
043500060331     C                   endif
043600060331     C*
043700060331     C                   if        wVATNOT_B <> *blanks
043800060331     C                   eval      VATNOT = wVATNOT_B
043900060331     C                   eval      VATTRC = 'B'
044000060331     C                   write     FIVAT000
044100060331     C                   add       1             §CTROKVT
044200060331     C                   endif
044300060331     C*
044400080625     C                   if        wVATNOT_E <> *blanks
044500060331     C                   eval      VATNOT = wVATNOT_E
044600060331     C                   eval      VATTRC = 'E'
044700080716     C                   write     FIVAT000
044800080716     C                   add       1             §CTROKVT
044900080625     C                   endif
045000040119     C*
045100040119     C                   ENDSR
045200040119
045300040119
045400040119
045500040119     C*----------------------------------------------------*
045600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045700040119     C*----------------------------------------------------*
045800040119     C     PREVAT        BEGSR
045900040119     C*
046000040119     C* Compongo il nome del membro da dare al FIVATWWR
046100040119     C                   eval      parmbr = vlrhdl
046200040119     C                   movel     'M'           parmbr
046300040510     C                   eval      parccm = vlrksc
046400040119     C                   eval      paropz = '1'
046500040119     C* Effettuo la chiamata al CLLE preposto
046600040119     C                   call(e)   'TITVVTC'
046700040119     C                   parm                    parccm
046800040119     C                   parm                    parmbr
046900040119     C                   parm                    paropz
047000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047100040119     C                   if        %error
047200040119     C                   movel     '1'           chkcall
047300040119     C                   else
047400040119     C                   movel     '0'           chkcall
047500040119     C                   endif
047600040119     C*
047700040119     C                   ENDSR
047800010601
047900010601
048000010601
048100010330     C*----------------------------------------------------*
048200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048300010330     C*----------------------------------------------------*
048400040119     C     INZVAR        BEGSR
048500010330     C*
048600030822     C* Inizializzo variabili di wrk
048700020204     C                   Z-ADD     *zeros        Num5_0            5 0
048800060331     C                   MOVEL     *blanks       wVATNOT_A
048900060331     C                   MOVEL     *blanks       wVATNOT_B
049000060331     C                   MOVEL     *blanks       wVATNOT_E
049100090220     C*
049200080617     C                   EVAL      wFlgCAS = *blanks
049300030822     C*
049400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049500030822     C                   CLEAR                   FIVAB000
049600040119     C                   CLEAR                   FIVAT000
049700030822     C                   CLEAR                   SkSplitCSV
049800030822     C                   EVAL      i = 1
049900030822     C                   EVAL      posDa = *zeros
050000030822     C                   EVAL      posA  = *zeros
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
051600030822     C*
051700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
051900030715     C                   SETON                                        50
052000030715     C                   ELSE
052100030715     C                   SETOFF                                       50
052200030715     C                   ENDIF
052300090421     C*
052400090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
052500090421     C                   EVAL      VABCBO = '1'
052600090421     C                   EVAL      VABLNP = vlrpoi
052700090421     C                   EVAL      VABNSP = rrnum
052800030822     C*
052900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053000030822     C                   EVAL      posDaDft = 1
053100030822     C                   EVAL      posADft  = 0
053200030822     C                   EVAL      wGiroDft = 0
053300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053400030822     C                             posDaDft > 0
053500030822     C*
053600030822     C* Gestisco il 1° giro
053700030822     C                   IF        wGiroDft = 0
053800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054000030822     C* Incremento il contatore dei "giri"
054100030822     C                   EVAL      wGiroDft = 1
054200030822     C                   ELSE
054300030822     C                   EVAL      posDaDft = posADft
054400030822     C                   ENDIF
054500030822     C* Eseguo lo scan x trovare la fine del campo corrente
054600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054700030822     C*
054800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055000030822     C                   IF        posDaDft > 0 AND
055100030822     C                             posADft  > 0
055200030822     C* NCL
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'NCL'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiInt=*on
056200030822     C                   Z-ADD     PiVal         VABNCL
056300030822     C                   ENDIF
056400030822     C                   ENDIF
056500030822     C* CCM
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'CCM'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABCCM
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* LNP
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'LNP'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABLNP
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* NRS
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'NRS'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABNRS
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* CTR
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'CTR'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABCTR
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* PKB
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'PKB'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiNum=*on
062700030822     C                   Z-ADD     PiVal         VABPKB
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* VLB
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'VLB'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000030822     C                   Z-ADD     PiVal         VABVLB
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* QFT
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'QFT'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiNum=*on
065300030822     C                   Z-ADD     PiVal         VABQFT
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* CBO
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'CBO'
066100030822     C                   EVAL      VABCBO=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* TSP
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'TSP'
067000030822     C                   EVAL      VABTSP=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400030822     C* VAS
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'VAS'
067900030822     C                   EVAL      VABVAS=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300030822     C* VCA
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'VCA'
068800030822     C                   EVAL      VABVCA=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200070925     C* TIC
069300070925     C                   IF        %subst(
069400070925     C                             %subst(vlrppt:posDaDft+1:
069500070925     C                             posADft-posDaDft-1):1:3)
069600070925     C                             = 'TIC'
069700070925     C                   EVAL      VABTIC=%trim(%subst(
069800070925     C                             %subst(vlrppt:posDaDft+1:
069900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000070925     C                   ENDIF
070100030822     C* GCA
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'GCA'
070600030822     C                   EVAL      VABGCA=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* CTM
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'CTM'
071500030822     C                   EVAL      VABCTM=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* FFD
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'FFD'
072400030822     C                   EVAL      VABFFD=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* VAD
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'VAD'
073300030822     C                   EVAL      VABVAD=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* GMA
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'GMA'
074200030822     C                   EVAL      VABGMA=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* GGA
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'GGA'
075100030822     C                   EVAL      VABGGA=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* GVA
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'GVA'
076000030822     C                   EVAL      VABGVA=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* TC1
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'TC1'
076900030822     C                   EVAL      VABTC1=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300030822     C* TC2
077400030822     C                   IF        %subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):1:3)
077700030822     C                             = 'TC2'
077800030822     C                   EVAL      VABTC2=%trim(%subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100030822     C                   ENDIF
078200040714     C* VATTRC
078300040714     C                   IF        %subst(
078400040714     C                             %subst(vlrppt:posDaDft+1:
078500040714     C                             posADft-posDaDft-1):1:3)
078600040714     C                             = 'TRC'
078700040714     C                   EVAL      VATTRC=%trim(%subst(
078800040714     C                             %subst(vlrppt:posDaDft+1:
078900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000040714     C                   ENDIF
079100030822     C* ...
079200030822     C                   ENDIF
079300030822     C                   ENDDO
079400020204     C*
079500010330     C                   ENDSR
079600010607     C*----------------------------------------------------*
079700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079800010607     C*----------------------------------------------------*
079900010607     C     IMPVAB        BEGSR
080000010607     C*
080100010607     C                   SETOFF                                       3132
080200030515     C*
080300010607     C                   MOVEL     datcor        VABAAS
080400010607     C                   MOVE      datcor        VABMGS
080500010607     C*
080600030715     C**********
080700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080800030715     C**********
080900030715     C                   IF        wGiro = *zeros
081000080716     C*
081100080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
081200080716     C                   EVAL      depvinDTA = vinDTA
081300040113     C*
081400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081600040510     C                             %trim(intcol(3))<>*blanks
081700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081800040113     C                                      %trim(intcol(3))
081900070823     C     ';':CharCSV   XLATE     vindta        vindta
082000040510     C                   ENDIF
082100080626     C*
082200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082300031201     C     minu:maiu     XLATE     vindta        vindta
082400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082500030715     C                   DOW       posDa <= %len(%trim(vindta))
082600030715     C*
082700030715     C* Gestisco il 1° campo
082800030715     C                   IF        i = 1
082900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083000030715     C                   EVAL      posDa = 1
083100030715     C* Eseguo lo scan x trovare la fine del primo campo
083200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083300030715     C* A questo punto "estraggo" il campo corrente
083400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083500030715     C                                                    (posA-posDa))
083600030715     C* X i campi successivi al 1°
083700030715     C                   ELSE
083800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083900030715     C                   EVAL      posDa = posA + 1
084000030715     C* Eseguo lo scan x trovare la fine del campo corrente
084100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084200030715     C* Gestisco l'ultimo campo
084300030715     C                   IF        posA = *zeros
084400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084500030715     C* A questo punto "estraggo" il campo corrente
084600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084700030715     C                                                    (posA-posDa))
084800030715     C                   LEAVE
084900030715     C                   ELSE
085000030715     C* A questo punto "estraggo" il campo corrente
085100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085200030715     C                                                    (posA-posDa))
085300030715     C                   ENDIF
085400030715     C                   ENDIF
085500030715     C* Incremento il contatore d campo
085600030715     C                   EVAL      i = i +1
085700030715     C                   ENDDO
085800030715     C*
085900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086000030715     C* delimitazione testo
086100030715     C                   EVAL      i = 1
086200030715     C                   DOW       i < %elem(SkSplitFLD)
086300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086500030715     C                   EVAL      i = i + 1
086600030715     C                   ENDDO
086700030715     C                   EVAL      wGiro = 1
086800080716     C* Ripristino la 1° riga originale
086900080716     C                   EVAL      vinDTA = depvinDTA
087000030715     C                   ELSE
087100030715     C                   EVAL      wGiro = 2
087200031204     C**********
087300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087400031204     C* d separatore campo
087500031204     C**********
087600031204     C                   EVAL      vindta = %trim(vindta)
087700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087800031204     C                   EVAL      vindta = %subst(vindta:2)
087900031204     C                   ENDDO
088000031204     C*
088100031204     C                   Z-ADD     *zeros        lunghInput        4 0
088200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088600031204     C                   ENDDO
088700030715     C**********
088800030715     C* Effettuo lo split del campo dati d input
088900030715     C**********
089000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089100030715     C                   DOW       posDa <= %len(%trim(vindta))
089200030715     C*
089300030715     C* Gestisco il 1° campo
089400030715     C                   IF        i = 1
089500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089600030715     C                   EVAL      posDa = 1
089700030715     C* Eseguo lo scan x trovare la fine del primo campo
089800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089900030715     C* A questo punto "estraggo" il campo corrente
090000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090100030715     C                                                    (posA-posDa))
090200030715     C* X i campi successivi al 1°
090300030715     C                   ELSE
090400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090500030715     C                   EVAL      posDa = posA + 1
090600031204     C* Verifico che nn vi sia il campo nn valorizzato
090700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090800031204     C* Se campo nn valorizzato skippo al prossimo
090900031204     C                   EVAL      SkSplitCSV(i) = *blanks
091000031204     C                   EVAL      posA  = posA + 1
091100031204     C                   ELSE
091200030715     C* Eseguo lo scan x trovare la fine del campo corrente
091300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091400030715     C* Gestisco l'ultimo campo
091500030715     C                   IF        posA = *zeros
091600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091700030715     C* A questo punto "estraggo" il campo corrente
091800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091900030715     C                                                    (posA-posDa))
092000030715     C                   LEAVE
092100030715     C                   ELSE
092200030715     C* A questo punto "estraggo" il campo corrente
092300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092400030715     C                                                    (posA-posDa))
092500030715     C                   ENDIF
092600031204     C                   ENDIF
092700030715     C                   ENDIF
092800030715     C* Incremento il contatore d campo
092900030715     C                   EVAL      i = i +1
093000030715     C                   ENDDO
093100030715     C*
093200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093300030715     C* delimitazione testo
093400030715     C                   EVAL      i = 1
093500030715     C                   DOW       i < %elem(SkSplitCSV)
093600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093800030715     C                   EVAL      i = i + 1
093900030715     C                   ENDDO
094000030715     C*
094100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094200030715     C                   EVAL      i = 1
094300030715     C                   DOW       i < %elem(SkSplitFLD)
094400030715     C*
094500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094700030715     C                             SkSplitCSV(i) <> *blanks
094800030721     C*** ATB
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095000030721     C                   EVAL      VABATB = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030715     C*** RSD
095300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095700030715     C* ==
095800030715     C                   ENDIF
095900030721     C*** RD2
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030715     C*** IND
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096500030715     C                   EVAL      VABIND = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030715     C*** LOD
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** PRD
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030721     C*** NZD
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030715     C*** RMA
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** NOT
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** NT2
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** VCA
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** VAS
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** FFD
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030721     C*** GC1
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** GC2
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** TSP
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101400030721     C                   ENDIF
101500040510     C*** CBO
101600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101800040510     C                   ENDIF
101900030721     C*** NAS
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** TIC
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** GCA
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** XCO
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** CTM
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TCR
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** CTS
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** FTM
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** VAD
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GMA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** GGA
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GVA
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TC1
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TC2
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** SCL
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** RMO
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** NMO
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*
108800030721     C* Reperisco quindi i campi numerici...
108900040510     C*** NSP
109000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109100040510     C                   EVAL      PiStr=SkSplitCSV(i)
109200040510     C                   EXSR      CHKNUM
109300040510     C                   IF        PiInt=*on
109400040510     C                   Z-ADD     PiVal         VABNSP
109500040510     C                   ELSE
109600040510     C                   SETON                                        31
109700040510     C                   EVAL      VABNSP = *zeros
109800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109900040510     C                             + ' ' + 'VABNSP'
110000040510     C                   ENDIF
110100040510     C                   ENDIF
110200071106     C*** RMN
110300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110400071106     C                   EVAL      PiStr=SkSplitCSV(i)
110500071106     C                   EXSR      CHKNUM
110600071106     C                   IF        PiInt=*on
110700071106     C                   Z-ADD     PiVal         VABRMN
110800071106     C                   ELSE
110900071106     C                   SETON                                        32
111000071106     C                   EVAL      VABRMN = *zeros
111100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111200071106     C                             + ' ' + 'VABRMN'
111300071106     C                   ENDIF
111400071106     C                   ENDIF
111500030721     C*** CAD
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111700030721     C                   EVAL      PiStr=SkSplitCSV(i)
111800030721     C                   EXSR      CHKNUM
111900030721     C                   IF        PiInt=*on
112000030721     C                   Z-ADD     PiVal         Num5_0
112100030721     C                   MOVEL(p)  Num5_0        VABCAD
112200030721     C                   ELSE
112300090603     C                   EVAL      VABCAD = SkSplitCSV(i)
112400090603     C                   SETON                                        32
112500090603     C                   EVAL      VABCAD = *zeros
112600090603     C                   EVAL      vinmsg = %trimr(vinmsg)
112700090603     C                             + ' ' + 'VABCAD'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** NCL
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiInt=*on
113500030721     C                   Z-ADD     PiVal         VABNCL
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      VABNCL = *zeros
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000040122     C                             + ' ' + 'VABNCL'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** PKB
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiNum=*on
114800030721     C                   Z-ADD     PiVal         VABPKB
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      VABPKB = *zeros
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300030721     C                             + ' ' + 'VABPKB'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** CAS
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115800080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
115900080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
116000081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
116100081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
116200080125     C                   EVAL      wFlgCAS = '1'
116300080125     C                   ENDIF
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiNum=*on
116700030721     C                   Z-ADD     PiVal         VABCAS
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABCAS = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABCAS'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** CCM
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiInt=*on      AND
118000030721     C                             PiVal<=9999999 AND
118100030721     C                             PiVal>*zeros
118200030721     C                   Z-ADD     PiVal         VABCCM
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABCCM = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700040510     C                             + ' ' + 'VABCCM'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** LNP
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on    AND
119500030721     C                             PiVal<=999   AND
119600030721     C                             PiVal>*zeros
119700030721     C                   Z-ADD     PiVal         VABLNP
119800030721     C                   ELSE
119900040715     C                   SETON                                        32
120000030721     C                   EVAL      VABLNP = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200040510     C                             + ' ' + 'VABLNP'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** NRS
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiInt=*on AND
121000030721     C                             PiVal<=99
121100030721     C                   Z-ADD     PiVal         VABNRS
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABNRS'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** LNA
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on    AND
122300030721     C                             PiVal<=999   AND
122400030721     C                             PiVal>*zeros
122500030721     C                   Z-ADD     PiVal         VABLNA
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      VABLNA = *zeros
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABLNA'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** CTR
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on  AND
123800030721     C                             PiVal<=999
123900030721     C                   Z-ADD     PiVal         VABCTR
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABCTR'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** IAS
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiNum=*on
125100030721     C                   Z-ADD     PiVal         VABIAS
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABIAS'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** VLB
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiNum=*on
126300030721     C                   Z-ADD     PiVal         VABVLB
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABVLB'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** QFT
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiNum=*on
127500030721     C                   Z-ADD     PiVal         VABQFT
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABQFT'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** NCD
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiInt=*on      AND
128700030721     C                             PiVal<=9999999
128800030721     C                   Z-ADD     PiVal         VABNCD
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABNCD'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** NCA
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiInt=*on      AND
130000030721     C                             PiVal<=9999999
130100030721     C                   Z-ADD     PiVal         VABNCA
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABNCA'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** ZNC
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiInt=*on AND
131300030721     C                             PiVal<=99
131400030721     C                   Z-ADD     PiVal         VABZNC
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABZNC'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** DCR
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on       AND
132600030721     C                             PiVal<=99999999
132700030721     C                   Z-ADD     PiVal         VABDCR
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABDCR'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** HCR
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on   AND
133900030721     C                             PiVal<=9999
134000030721     C                   Z-ADD     PiVal         VABHCR
134100030721     C                   ELSE
134200030721     C                   SETON                                        32
134300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134400030721     C                             + ' ' + 'VABHCR'
134500030721     C                   ENDIF
134600030721     C                   ENDIF
134700030721     C*** VMD
134800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134900030721     C                   EVAL      PiStr=SkSplitCSV(i)
135000030721     C                   EXSR      CHKNUM
135100030721     C                   IF        PiNum=*on
135200030721     C                   Z-ADD     PiVal         VABVMD
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABVMD'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** ANT
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on        AND
136400030721     C                             PiVal<=999999999
136500030721     C                   Z-ADD     PiVal         VABANT
136600030721     C                   ELSE
136700030721     C                   SETON                                        32
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABANT'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200030721     C*** CMO
137300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiInt=*on
137700030721     C                   Z-ADD     PiVal         Num5_0
137800030721     C                   MOVEL(p)  Num5_0        VABCMO
137900030721     C                   ELSE
138000030721     C                   SETON                                        32
138100030721     C                   EVAL      VABCMO = *zeros
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300030721     C                             + ' ' + 'VABCMO'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600040510     C*
138700040510     C* Reperisco i campi del FIVAT
138800040510     C*** VATNOT
138900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139100040510     C                   ENDIF
139200040510     C*** VATTRC
139300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139500040510     C                   ENDIF
139600060331     C*** VATNOT_A
139700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139900060331     C                   ENDIF
140000060331     C*** VATNOT_B
140100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140300060331     C                   ENDIF
140400060331     C*** VATNOT_E
140500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140700060331     C                   ENDIF
140800070823     C*
140900071029     C***  ===>  Gestione campi "particolari" dentro ciclo
141000090421     C*
141100090622     C*** W_VABRSD
141200090622     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141300090622     C                   MOVEL     *blanks       wRSD             70
141400090622     C                   MOVEL     SkSplitCSV(i) wRSD
141500090622     C                   EVAL      VABRSD = %subst(wRSD:1:35)
141600090622     C                   EVAL      VABRD2 = %subst(wRSD:36:35)
141700090622     C                   ENDIF
141800090622     C*** W_VABNOT
141900090622     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
142000090622     C                   MOVEL     *blanks       wNOTE            70
142100090622     C                   MOVEL     SkSplitCSV(i) wNOTE
142200090622     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
142300090622     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
142400090622     C                   ENDIF
142500090622     C*** W_VABRMN
142600090622     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142700090622     C                   EVAL      VABRMA = SkSplitCSV(i)
142800090622     C                   EVAL      PiStr=SkSplitCSV(i)
142900090622     C                   EXSR      CHKNUM
143000090622     C                   IF        PiInt=*on
143100090622     C                   Z-ADD     PiVal         VABRMN
143200090622     C                   Z-ADD     PiVal         VABNSP
143300090622     C                   ELSE
143400090622     C                   SETON                                        32
143500090622     C                   EVAL      VABRMN = *zeros
143600090622     C                   EVAL      VABNSP = *zeros
143700090622     C                   EVAL      vinmsg = %trimr(vinmsg)
143800090622     C                             + ' ' + 'VABRMN VABNSP'
143900090622     C                   ENDIF
144000090603     C                   ENDIF
144100070925     C*
144200071029     C***  <===  -----------------------------------------
144300040510     C*
144400030715     C                   ENDIF
144500030715     C                   EVAL      i = i + 1
144600030715     C                   ENDDO
144700070925     C*
144800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144900080125     C*
145000070925     C*
145100071029     C***  <===  -----------------------------------------
145200070925     C*
145300070925     C* Considerazioni finali su CBO/CAS
145400080125     C                   IF        wFlgCAS = '1'
145500070925     C                   IF        VABCBO = '1'
145600070925     C                   EVAL      VABCBO = '4'
145700070925     C                   ENDIF
145800070925     C                   IF        VABCBO = '2'
145900070925     C                   EVAL      VABCBO = '6'
146000070925     C                   ENDIF
146100070925     C                   ENDIF
146200070925     C***  <===  ----------------------------
146300040510     C*
146400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146500040510     C                   IF        VABNSP = *zeros
146600040510     C                   SETON                                        31
146700040510     C                   ENDIF
146800040715     C*
146900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147000040715     C                   IF        VABLNP = *zeros
147100040715     C                   SETON                                        31
147200040715     C                   ENDIF
147300040510     C*
147400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147500040510     C                   IF        VABRMN = *zeros
147600040510     C                   Z-ADD     VABNSP        VABRMN
147700040510     C                   ENDIF
147800040510     C*
147900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148000040510     C                   IF        VABRMA = *blanks
148100040714     C                   MOVEL     VABRMN        VABRMA
148200040510     C                   ENDIF
148300040128     C*
148400040510     C* Se provincia nn valorizzata la reperisco
148500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148600040510     C                   IF        VABPRD = *blanks
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
155900080617
156000080617
156100080617
156200080617     C*----------------------------------------------------*
156300080617     C*  CONTROLLO NUMERICITA' CAMPI
156400080617     C*----------------------------------------------------*
156500080617     C     CHKNUM        BEGSR
156600080617     C*
156700080617     C                   IF        PiDecChr = *blanks
156800080617     C                   EVAL      PiDecChr = CharNUM
156900080617     C                   ENDIF
157000080617     C*
157100080617     C                   callp     UBISNUM_Check(PiStr
157200080617     C                                          :PiDecChr
157300080617     C                                          :PiVal
157400080617     C                                          :PiNum
157500080617     C                                          :PiInt)
157600080617     C*
157700080617     C                   ENDSR
157800080617     C***
157900010330
158000010601
158100010601
158200010601
158300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158400010601     C     repfil        BEGSR
158500010601     C*
158600010601     C                   if        invfil = *zeros and
158700010601     C                             depfil > *zeros and
158800010629     C                             (vinflg = *blanks or
158900010629     C                              vinflg = *zeros)
159000010601     C
159100010601     C                   eval      invfil = depfil
159200010601     C                   endif
159300010601     C*
159400010601     C                   if        depfil <> invfil and
159500010601     C                             invfil > *zeros
159600010601     C                   eval      flgMulti = '1'
159700010601     C                   if        vinflg = *blanks
159800010601     C                   add       1             cntNonEl
159900010601     C                   endif
160000010601     C                   endif
160100010601     C*
160200010601     C                   if        vinflg = '2'
160300010601     C                   eval      flgStato = '2'
160400010601     C                   endif
160500010601     C*
160600010601     C                   ENDSR
160700010601     C***
160800010601
160900010601
161000010601
161100010330
161200040119      /TITLE Invio dei dati al punto operativo.
161300040119     C     invio         BEGSR
161400040119     C*
161500040119     C* 1° invio FIVAT
161600040119     C                   reset                   dscmz
161700040119     C                   move      vlrpoi        cmzdst
161800040119     C                   eval      cmzfld = 'FIVATWWR'
161900040119     C                   eval      cmzmbd = vlrhdl
162000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162100040119     C***                if        prmfir = *blanks
162200040119     C                   eval      cmzfla = 'FIVAT00F'
162300040119     C                   eval      cmzmba = 'FIVAT00F'
162400040119     C***                else
162500040119     C***                eval      cmzfla = prmfir
162600040119     C***                eval      cmzmba = prmfir
162700040119     C***                endif
162800040119     C                   eval      cmznrr = *zeros
162900040119     C                   move      §ctrokvt      cmznrr
163000040119     C                   eval      cmzlba = vlrfl1
163100040119     C                   call(e)   'TIS711C'
163200040119     C                   parm                    dscmz
163300040119     C                   parm      *blanks       esito
163400040119     C                   if        %error
163500040119     C                             or cmzerr = '1'
163600040119     C                             or esito  = '1'
163700040119     C                   eval      wrkesito = '3'
163800040119     C                   else
163900040119     C*
164000040119     C* 2° invio FIVAB
164100040119     C                   reset                   dscmz
164200040119     C                   move      vlrpoi        cmzdst
164300040119     C                   eval      cmzfld = vlrfou
164400040119     C                   eval      cmzmbd = vlrhdl
164500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164600040119     C***                if        prmfir = *blanks
164700040119     C                   eval      cmzfla = 'FIVAB00F'
164800040119     C                   eval      cmzmba = 'FIVAB00F'
164900040119     C***                else
165000040119     C***                eval      cmzfla = prmfir
165100040119     C***                eval      cmzmba = prmfir
165200040119     C***                endif
165300040119     C                   eval      cmznrr = *zeros
165400040119     C                   move      §ctrokvb      cmznrr
165500040119     C                   eval      cmzlba = vlrfl1
165600040119     C                   call(e)   'TIS711C'
165700040119     C                   parm                    dscmz
165800040119     C                   parm      *blanks       esito
165900040119     C                   if        %error
166000040119     C                             or cmzerr = '1'
166100040119     C                             or esito  = '1'
166200040119     C                   eval      wrkesito = '3'
166300040119     C                   endif
166400040119     C                   endif
166500040119     C*
166600040119     C                   ENDSR
166700040119     C***
166800010601
166900010601
167000010601
167100010601
167200010601      /TITLE Invio dei dati al punto operativo.
167300010601     C     opeini        BEGSR
167400010601     C*
167500010601     C* Inizializzo flag e contatori operativi
167600010601     C                   movel     '0'           flgGiro           1
167700010601     C                   movel     '0'           flgMulti          1
167800010601     C                   movel     '1'           flgStato          1
167900010615     C                   movel     '0'           flgOk             1
168000010601     C                   z-add     *zeros        cntNonEl         10 0
168100010601     C                   z-add     *zeros        depfil            3 0
168200010601     C                   z-add     *zeros        invfil            3 0
168300010601     C*
168400010601     C                   ENDSR
168500010601     C***
168600070326
168700070326
168800070326
168900070326
169000080916     C     *pssr         BEGSR
169100070329     C*
169200080916     C                   if        %open(tivin00r)
169300080916     C                   close     tivin00r
169400080916     C                   endif
169500080916     C                   if        %open(fivabwwr)
169600080916     C                   close     fivabwwr
169700080916     C                   endif
169800080916     C                   if        %open(fivatwwr)
169900080916     C                   close     fivatwwr
170000080916     C                   endif
170100070326     C*
170200070326     C* Effettuo la chiamata al CLLE preposto
170300080916     C                   call(e)   'TITVVTC'
170400080916     C                   parm                    parccm
170500080916     C                   parm                    parmbr
170600080916     C                   parm      '2'           paropz
170700070326     C*
170800080916     C                   eval      wrkesito = '2'
170900070404     C*
171000080916     C                   seton                                        LR
171100070326     C*
171200080916     C                   ENDSR     '*CANCL'
171300070326     C***
171400070326
171500070326
171600010330
171700010330
171800000613     C     *inzsr        BEGSR
171900990910     C*
172000990910     C     *entry        plist
172100990920     C                   parm                    tivlrds
172200990921     C                   parm      wrkesito      esito
172300000724     C                   parm                    prmlit
172400000710     C                   parm                    prmfir
172500010330     C*
172600010330     C* CALCOLA LA DATA CORRENTE
172700010330     C                   time                    wn14             14 0
172800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
172900010330     C                   z-add     wn8           g08dat
173000010330     C                   z-add     *zeros        g08inv
173100010330     C                   movel     '0'           g08err
173200010330     C                   call      'XSRDA8'
173300010330     C                   parm                    wlbda8
173400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
173500000613     C*
173600000613     C                   ENDSR
173700000613     C***
173800990908
173900090622     Otitv1q6p  E            riepilogo         2
174000990915     O                                              'Upload via Internet'
174100990915     O                                           +1 'Traduzione TIVIN00R -'
174200021025     O                                           +1 'FIVABWWR'
174300990915     O                                           +1 'Testate bolle'
174400990915     O          E            riepilogo   2
174500990915     O                       wrkdata
174600990915     O                       wrkora              +1
174700990915     O                       procname            +1
174800990915     O          E            riepilogo   2
174900990915     O                                              'Cliente..................:'
175000010605     O                       VABCCM        z     +1
175100990915     O          E            riepilogo   2
175200990920     O                                              'Riferimento Strategi.....:'
175300990920     O                       vlrhdl              +1
175400990915     O          E            riepilogo   2
175500990915     O                                              'Giusti...................:'
175600040119     O                       §CTROKVB      2   +  1
175700990915     O          E            riepilogo   2
175800990915     O                                              'Sbagliati e corretti.....:'
175900971022     O                       §CTRMO        2   +  1
176000990915     O          E            riepilogo   2
176100990915     O                                              'Sbagliati e scartati.....:'
176200971022     O                       §CTRNO        2   +  1
176300000613
176400090622     Otitv1q6ps E            testdett          2
176500000613     O                                              'Upload via Internet'
176600000613     O                                           +1 'Traduzione TIVIN00R -'
176700021025     O                                           +1 'FIVABWWR'
176800000613     O                                           +1 'Testate bolle'
176900000616     O          E            testdett    3
177000000613     O                                           +2 'N° rec'
177100000613     O                                           +3 'Anteprima contenuto'
177200000616     O          E            rigadett    2
177300000613     O                       rrnum               +2
177400000621     O                       recko               +3
177500000616     O          E            findett     2
177600000613     O                       wrkdata
177700000613     O                       wrkora              +1
177800000613     O                       procname            +1
177900000616     O          E            findett     2
178000000613     O                                              'Cliente..................:'
178100010605     O                       VABCCM        z     +1
178200000616     O          E            findett     2
178300000613     O                                              'Riferimento Strategi.....:'
178400000613     O                       vlrhdl              +1
178500000616     O          E            findett     2
178600000613     O                                              'Giusti...................:'
178700040119     O                       §CTROKVB      2   +  1
178800000616     O          E            findett     2
178900000613     O                                              'Sbagliati e corretti.....:'
179000000613     O                       §CTRMO        2   +  1
179100000616     O          E            findett     2
179200000613     O                                              'Sbagliati e scartati.....:'
179300000613     O                       §CTRNO        2   +  1
179400000613** CMD - COMANDI CL
179500090622OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
179600090622OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
179700090622DLTOVR FILE(TITV1Q6P TITV1Q6PS) LVL(*)
179800010606
179900000613
180000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180100090622VABCCM;VABLNP;
180200090622FILL001;W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;VABCTR;VABTSP;
180300090622VABIAS;VABNCL;VABPKB;VABCAS;VABTIC;W_VABRMN;W_VABNOT;VATNOT_B;VABTC1
