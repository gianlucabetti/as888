000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800090921     Ftitv1r8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100090921     Ftitv1r8ps O    f  198        PRINTER usropn
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
008300090921     D* DS REPERIMENTO NUMERATORE
008400090921     D*------------------
008500090921     D trul33ds      e ds                  inz
008600090921     D*------------------
008700090921     D* DS ARCHITETTURA
008800090921     D*------------------
008900090921     D kpjba         e ds                  inz
009000090921     D*------------------
009100080617
009200080617     D*------------------
009300080617     D* LINKING A DEFINIZIONI ESTERNE
009400080617     D*------------------
009500080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009600090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009700010330
009800010330
009900010330
010000990915     C                   time                    wrkdata
010100990915     C                   time                    wrkora
010200000913     C                   reset                   rrnum
010300990921     C                   reset                   esito
010400081014     C                   reset                   wrkesito
010500010601     C*
010600010601     C                   exsr      opeini
010700010605     C                   exsr      rwvab
010800081014     C*
010900081014     C                   exsr      endela
011000010601     C*
011100010601     C                   seton                                        lr
011200010601
011300010601
011400010601
011500010601     C*--------------------------------------------------------
011600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011700010601     C*--------------------------------------------------------
011800010601     C     PREELA        BEGSR
011900010601     C*
012000010601     C* SE OCCORRE SPEDIRE IN FILIALE
012100010601     C                   if        invfil <> *zeros and
012200010601     C                             flgGiro = '0'
012300010601     C*
012400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012500010601     C                   eval      flgGiro = '1'
012600000724     C*
012700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012900010601     C     invfil        chain     azorg01l
013000000724     C                   if        %found
013100000724     C                   movel(p)  CMD(1)        depcmd
013200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013300000724     C*
013400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013500011113     C                   Z-ADD     150           LENGH            15 5
013600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013700000724     C                   PARM                    depcmd
013800000724     C                   PARM                    LENGH
013900000724     C*
014000000724     C                   endif
014100000616     C*
014200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014300000616     C                   movel(p)  CMD(2)        depcmd
014400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014500000616     C*
014600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014700011113     C                   Z-ADD     150           LENGH            15 5
014800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014900000616     C                   PARM                    depcmd
015000000616     C                   PARM                    LENGH
015100000613     C*
015200090921     C                   if        not %open(titv1r8ps)
015300090921     C                   open      titv1r8ps
015400070329     C                   except    testdett
015500000613     C                   endif
015600010601     C*
015700010601     C                   endif
015800010601     C*
015900010601     C                   ENDSR
016000010601     C***
016100010601
016200010601
016300010601
016400010601
016500010601     C*--------------------------------------------------------
016600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016700010601     C*--------------------------------------------------------
016800010601     C     ENDELA        BEGSR
016900081014     C*
017000081014     C                   if        %open(tivin00r)
017100081014     C                   close     tivin00r
017200081014     C                   endif
017300081014     C                   if        %open(fivabwwr)
017400081014     C                   close     fivabwwr
017500081014     C                   endif
017600081014     C                   if        %open(fivatwwr)
017700081014     C                   close     fivatwwr
017800081014     C                   endif
017900040119     C*
018000040119     C* Effettuo la chiamata al CLLE preposto
018100040119     C                   call(e)   'TITVVTC'
018200040510     C                   parm                    parccm
018300040119     C                   parm                    parmbr
018400040119     C                   parm      '2'           paropz
018500050201     C*
018600050201     C* Effettuo lancio TISI95 solo x chiusura
018700050201     C                   CLEAR                   TISI95DS
018800050201     C                   EVAL      I95TLA = 'C'
018900050201     C                   CALL      'TISI95R'
019000050201     C                   PARM                    TISI95DS
019100000613     C*
019200090921     C                   if        %open(titv1r8ps)
019300000616     C                   except    findett
019400090921     C                   close     titv1r8ps
019500000613     C                   endif
019600000616     C*
019700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019800011113     C                   Z-ADD     150           LENGH            15 5
019900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020000000616     C                   PARM                    CMD(3)
020100000616     C                   PARM                    LENGH
020200000616     C*
020300010601     C                   ENDSR
020400010601     C***
020500010601
020600010601
020700010601
020800000613
020900000613     C*--------------------------------------------------------
021000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021100000613     C*--------------------------------------------------------
021200000621     C     STPR          BEGSR
021300000613     C*
021400090921     C                   if        not %open(titv1r8p)
021500090921     C   50              open      titv1r8p
021600990915     C                   endif
021700990915     C*
021800030715     C   50              except    riepilogo
021900990915     C*
022000090921     C                   if        %open(titv1r8p)
022100090921     C   50              close     titv1r8p
022200990914     C                   endif
022300000613     C*
022400000613     C                   ENDSR
022500000613     C***
022600000613
022700010601
022800010601
022900010330     C*--------------------------------------------------------
023000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023100010330     C*--------------------------------------------------------
023200010605     C     RWVAB         BEGSR
023300010330     C*
023400010330     C                   if        not %open(tivin00r)
023500010330     C                   open      tivin00r
023600010330     C                   endif
023700021025     C                   if        not %open(fivabwwr)
023800021025     C                   open      fivabwwr
023900010330     C                   endif
024000040119     C*
024100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024200040119     C                   exsr      prevat
024300040119     C*
024400040119     C                   if        chkcall = '0'
024500040119     C*
024600040119     C                   if        not %open(fivatwwr)
024700040119     C                   open      fivatwwr
024800040119     C                   endif
024900010330     C*
025000040119     C                   clear                   §CTROKVB          7 0
025100040119     C                   clear                   §CTROKVT          7 0
025200040119     C                   clear                   §CTRMO            7 0
025300040119     C                   clear                   §CTRNO            7 0
025400070823     C*
025500070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025600070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025700090420     C                   exsr      inzvar
025800090420     C                   exsr      impvab
025900010330     C*
026000030822     C                   do        *HIVAL
026100010330     C*
026200030822     C                   read      tivin00r                               70
026300010618     C*
026400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026500010618     C                   if        vinflg = '1'
026600010618     C                   eval      flgOk = '1'
026700010618     C                   endif
026800040510     C*
026900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027000040510     C                   z-add     *zeros        wLenVINDTA        4 0
027100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027500040510     C                   z-add     1             wFlgVINDTA
027600040510     C                   leave
027700040510     C                   endif
027800040510     C                   enddo
027900090420     C*
028000090420     C                   add       1             rrnum
028100010618     C*
028200040510     C                   if        vindta > *blanks AND
028300090420     C                             wFlgVINDTA = 1
028400010330     C*
028500010601     C                   if        *in70 = *off and
028600010330     C                             (vinflg = *blanks
028700010330     C                              or vinflg = '0'
028800010330     C                              or vinflg = '2')
028900010330     C*
029000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029100010711     C                   if        vinflg = *blanks or vinflg = '0'
029200010711     C                   clear                   vinmsg
029300010711     C                   endif
029400010601     C*
029500090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
029600090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
029700090421     C                   eval      vindta  = '*' + %trim(vindta)
029800090421     C                   endif
029900070925     C*
030000010330     C                   exsr      inzvar
030100010605     C                   exsr      impvab
030200010601     C*
030300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030400010605     C                   eval      depfil = VABLNP
030500010601     C                   exsr      repfil
030600010601     C                   if        depfil = invfil
030700021025     C                   if        vlrpoi = 999
030800030822     C                   move(P)   invfil        VABFGS
030900021025     C                   else
031000030822     C                   move(P)   vlrpoi        VABFGS
031100021025     C                   endif
031200010601     C*
031300010601     C                   exsr      PREELA
031400010601     C*
031500010604     C* Ebbene...
031600010604     C*
031700040119     C  N31              add       1             §CTROKVB
031800040119     C   32              add       1             §CTRMO
031900040119     C   31              add       1             §CTRNO
032000030822     C                   if        wGiro = 2
032100040510     C* Scrivo sempre il VAT
032200040510     C  N31              exsr      wrivat
032300040510     C* Scrivo il VAB solo a rottura d spedizione
032400040119     C                   if        depspe =  *zeros OR
032500040119     C                             depspe <> vabNSP
032600040119     C  N31              exsr      wrivab
032700040119     C  N31              eval      depspe = vabNSP
032800040119     C                   endif
032900040119     C                   endif
033000010604     C*
033100010604     C                   if        *in31 = *off and
033200010604     C                             *in32 = *off
033300010604     C                   eval      vinflg = '1'
033400010604     C                   else
033500010604     C                   eval      recko = vindta
033600010604     C                   except    rigadett
033700010604     C                   eval      vinflg = '2'
033800010604     C                   endif
033900010604     C                   endif
034000010601     C*
034100010604     C                   endif
034200010604     C*
034300010330     C                   else
034400010330     C                   eval      vinflg = '1'
034500010330     C                   endif
034600010601     C*
034700010601     C  N70              update    tivin000
034800010330     C*
034900030822     C  N70              enddo
035000040119     C*
035100040119     C                   endif                                                  (endif - chkcall)
035200010601     C*
035300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035400010601     C                   if        cntNonEl = *zeros or
035500010601     C                             flgMulti = '0'
035600010330     C* Se non ci sono record con errori ...
035700010601     C                   if        §ctrno = 0 and
035800010604     C                             §ctrmo = 0 and
035900010601     C                             flgStato <> '2'
036000010330     C* ... restituisco esito OK.
036100010330     C                   eval      wrkesito = '0'
036200010330     C                   else
036300040119     C                   if        §ctrokvb > 0
036400010330     C                   eval      wrkesito = '1'
036500010330     C                   else
036600010615     C                   if        flgOk = '0'
036700010615     C                   eval      wrkesito = '2'
036800010615     C                   else
036900010615     C                   eval      wrkesito = '6'
037000010615     C                   endif
037100010330     C                   endif
037200010330     C                   endif
037300010601     C                   else
037400010601     C                   eval      wrkesito = '9'
037500010601     C                   endif
037600010330     C*
037700010330     C                   if        %open(tivin00r)
037800010330     C                   close     tivin00r
037900010330     C                   endif
038000021025     C                   if        %open(fivabwwr)
038100021025     C                   close     fivabwwr
038200010330     C                   endif
038300040512     C                   if        %open(fivatwwr)
038400040512     C                   close     fivatwwr
038500040512     C                   endif
038600010601     C*
038700010601     C                   if        vlrpoi <> 999
038800010601     C                   eval      invfil = vlrpoi
038900010601     C                   endif
039000010330     C*
039100040119     C                   if        §ctrokvb > 0
039200010601     C                             and invfil > *zeros
039300010330     C                   exsr      invio
039400010330     C                   endif
039500010601     C*
039600010618     C                   if        flgGiro = '1'
039700081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039800010618     C                   endif
039900010330     C*
040000010330     C                   ENDSR
040100010330     C***
040200040119
040300040119
040400040119
040500040119     C*----------------------------------------------------*
040600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040700040119     C*----------------------------------------------------*
040800040510     C     WRIVAB        BEGSR
040900040119     C*
041000060331     C* Gestisco l'eventuale rottura x numero spedizione
041100070823     C                   if        VABNSP <> depspe
041200070301     C                   if        wVATNOT_E <> *blanks
041300080125     C                   if        VABCTM = *blanks
041400080125     C                   eval      VABCTM = '7Q'
041500080125     C                   endif
041600070301     C                   endif
041700040119     C                   write     fivab000                                     => scarico il VAB
041800060331     C                   endif
041900040119     C*
042000040119     C                   ENDSR
042100040119
042200040119
042300040119
042400040119     C*----------------------------------------------------*
042500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042600040119     C*----------------------------------------------------*
042700040510     C     WRIVAT        BEGSR
042800060512     C*
042900060512     C* Inizializzo i campi chiave
043000060512     C                   eval      VATFGS = VABFGS
043100060512     C                   eval      VATAAS = VABAAS
043200060512     C                   eval      VATCCM = VABCCM
043300070823     C                   eval      VATNRS = VABNRS
043400060512     C                   eval      VATNSP = VABNSP
043500060512     C                   eval      VATLNP = VABLNP
043600060331     C*
043700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043800060331     C                   if        wVATNOT_A <> *blanks
043900091001     C                   eval      VATNOT = %trim(wVATNOT_A)
044000060331     C                   eval      VATTRC = 'A'
044100060331     C                   write     FIVAT000
044200060331     C                   add       1             §CTROKVT
044300060331     C                   endif
044400060331     C*
044500060331     C                   if        wVATNOT_B <> *blanks
044600091001     C                   eval      VATNOT = %trim(wVATNOT_B)
044700060331     C                   eval      VATTRC = 'B'
044800060331     C                   write     FIVAT000
044900060331     C                   add       1             §CTROKVT
045000060331     C                   endif
045100060331     C*
045200080625     C                   if        wVATNOT_E <> *blanks
045300091001     C                   eval      VATNOT = %trim(wVATNOT_E)
045400060331     C                   eval      VATTRC = 'E'
045500080716     C                   write     FIVAT000
045600080716     C                   add       1             §CTROKVT
045700080625     C                   endif
045800040119     C*
045900040119     C                   ENDSR
046000040119
046100040119
046200040119
046300040119     C*----------------------------------------------------*
046400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046500040119     C*----------------------------------------------------*
046600040119     C     PREVAT        BEGSR
046700040119     C*
046800040119     C* Compongo il nome del membro da dare al FIVATWWR
046900040119     C                   eval      parmbr = vlrhdl
047000040119     C                   movel     'M'           parmbr
047100040510     C                   eval      parccm = vlrksc
047200040119     C                   eval      paropz = '1'
047300040119     C* Effettuo la chiamata al CLLE preposto
047400040119     C                   call(e)   'TITVVTC'
047500040119     C                   parm                    parccm
047600040119     C                   parm                    parmbr
047700040119     C                   parm                    paropz
047800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047900040119     C                   if        %error
048000040119     C                   movel     '1'           chkcall
048100040119     C                   else
048200040119     C                   movel     '0'           chkcall
048300040119     C                   endif
048400040119     C*
048500040119     C                   ENDSR
048600010601
048700010601
048800010601
048900010330     C*----------------------------------------------------*
049000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049100010330     C*----------------------------------------------------*
049200040119     C     INZVAR        BEGSR
049300010330     C*
049400030822     C* Inizializzo variabili di wrk
049500020204     C                   Z-ADD     *zeros        Num5_0            5 0
049600090921     C                   MOVEL     *blanks       wRSD             70
049700060331     C                   MOVEL     *blanks       wVATNOT_A
049800060331     C                   MOVEL     *blanks       wVATNOT_B
049900060331     C                   MOVEL     *blanks       wVATNOT_E
050000090220     C*
050100080617     C                   EVAL      wFlgCAS = *blanks
050200030822     C*
050300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050400030822     C                   CLEAR                   FIVAB000
050500040119     C                   CLEAR                   FIVAT000
050600030822     C                   CLEAR                   SkSplitCSV
050700030822     C                   EVAL      i = 1
050800030822     C                   EVAL      posDa = *zeros
050900030822     C                   EVAL      posA  = *zeros
051000030822     C*
051100030822     C* Reimposto i valori di default
051200030822     C                   EXSR      DEFCAM
051300030822     C*
051400010330     C                   ENDSR
051500010330     C*----------------------------------------------------*
051600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051700010330     C*----------------------------------------------------*
051800010330     C     DEFCAM        BEGSR
051900030822     C*
052000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052100030715     C* e delimitatore testo.
052200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052500030822     C*
052600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
052800030715     C                   SETON                                        50
052900030715     C                   ELSE
053000030715     C                   SETOFF                                       50
053100030715     C                   ENDIF
053200090421     C*
053300090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
053400090421     C                   EVAL      VABCBO = '1'
053500090421     C                   EVAL      VABLNP = vlrpoi
053600090421     C                   EVAL      VABNSP = rrnum
053700091026     C*
053800091026     C*** PERSONALIZZAZIONE CLIENTE ***
053900091026     C                   MOVEL     *blanks       wNOTE            70
054000091026     C                   EVAL      wNOTE = 'RICHIESTA VARIAZIONE INDIRIZZO ' +
054100091026     C                             'E FERMO DEPOSITO AUTORIZZATO -NO GIACENZA'
054200091026     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
054300091026     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
054400091026     C*********************************
054500030822     C*
054600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054700030822     C                   EVAL      posDaDft = 1
054800030822     C                   EVAL      posADft  = 0
054900030822     C                   EVAL      wGiroDft = 0
055000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055100030822     C                             posDaDft > 0
055200030822     C*
055300030822     C* Gestisco il 1° giro
055400030822     C                   IF        wGiroDft = 0
055500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055700030822     C* Incremento il contatore dei "giri"
055800030822     C                   EVAL      wGiroDft = 1
055900030822     C                   ELSE
056000030822     C                   EVAL      posDaDft = posADft
056100030822     C                   ENDIF
056200030822     C* Eseguo lo scan x trovare la fine del campo corrente
056300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056400030822     C*
056500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056700030822     C                   IF        posDaDft > 0 AND
056800030822     C                             posADft  > 0
056900030822     C* NCL
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'NCL'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900030822     C                   Z-ADD     PiVal         VABNCL
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* CCM
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'CCM'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiInt=*on
059200030822     C                   Z-ADD     PiVal         VABCCM
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* LNP
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'LNP'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500030822     C                   Z-ADD     PiVal         VABLNP
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* NRS
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'NRS'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiInt=*on
061800030822     C                   Z-ADD     PiVal         VABNRS
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* CTR
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'CTR'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiInt=*on
063100030822     C                   Z-ADD     PiVal         VABCTR
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* PKB
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'PKB'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiNum=*on
064400030822     C                   Z-ADD     PiVal         VABPKB
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* VLB
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'VLB'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiNum=*on
065700030822     C                   Z-ADD     PiVal         VABVLB
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* QFT
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'QFT'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiNum=*on
067000030822     C                   Z-ADD     PiVal         VABQFT
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* CBO
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'CBO'
067800030822     C                   EVAL      VABCBO=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* TSP
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'TSP'
068700030822     C                   EVAL      VABTSP=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* VAS
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'VAS'
069600030822     C                   EVAL      VABVAS=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* VCA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'VCA'
070500030822     C                   EVAL      VABVCA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900070925     C* TIC
071000070925     C                   IF        %subst(
071100070925     C                             %subst(vlrppt:posDaDft+1:
071200070925     C                             posADft-posDaDft-1):1:3)
071300070925     C                             = 'TIC'
071400070925     C                   EVAL      VABTIC=%trim(%subst(
071500070925     C                             %subst(vlrppt:posDaDft+1:
071600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700070925     C                   ENDIF
071800030822     C* GCA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'GCA'
072300030822     C                   EVAL      VABGCA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* CTM
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'CTM'
073200030822     C                   EVAL      VABCTM=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* FFD
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'FFD'
074100030822     C                   EVAL      VABFFD=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VAD
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VAD'
075000030822     C                   EVAL      VABVAD=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GMA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GMA'
075900030822     C                   EVAL      VABGMA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* GGA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GGA'
076800030822     C                   EVAL      VABGGA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* GVA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GVA'
077700030822     C                   EVAL      VABGVA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* TC1
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'TC1'
078600030822     C                   EVAL      VABTC1=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* TC2
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'TC2'
079500030822     C                   EVAL      VABTC2=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900040714     C* VATTRC
080000040714     C                   IF        %subst(
080100040714     C                             %subst(vlrppt:posDaDft+1:
080200040714     C                             posADft-posDaDft-1):1:3)
080300040714     C                             = 'TRC'
080400040714     C                   EVAL      VATTRC=%trim(%subst(
080500040714     C                             %subst(vlrppt:posDaDft+1:
080600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700040714     C                   ENDIF
080800030822     C* ...
080900030822     C                   ENDIF
081000030822     C                   ENDDO
081100020204     C*
081200010330     C                   ENDSR
081300010607     C*----------------------------------------------------*
081400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081500010607     C*----------------------------------------------------*
081600010607     C     IMPVAB        BEGSR
081700010607     C*
081800010607     C                   SETOFF                                       3132
081900030515     C*
082000010607     C                   MOVEL     datcor        VABAAS
082100010607     C                   MOVE      datcor        VABMGS
082200010607     C*
082300030715     C**********
082400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082500030715     C**********
082600030715     C                   IF        wGiro = *zeros
082700080716     C*
082800080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
082900080716     C                   EVAL      depvinDTA = vinDTA
083000040113     C*
083100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083300040510     C                             %trim(intcol(3))<>*blanks
083400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083500040113     C                                      %trim(intcol(3))
083600070823     C     ';':CharCSV   XLATE     vindta        vindta
083700040510     C                   ENDIF
083800080626     C*
083900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084000031201     C     minu:maiu     XLATE     vindta        vindta
084100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084200030715     C                   DOW       posDa <= %len(%trim(vindta))
084300030715     C*
084400030715     C* Gestisco il 1° campo
084500030715     C                   IF        i = 1
084600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084700030715     C                   EVAL      posDa = 1
084800030715     C* Eseguo lo scan x trovare la fine del primo campo
084900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085000030715     C* A questo punto "estraggo" il campo corrente
085100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085200030715     C                                                    (posA-posDa))
085300030715     C* X i campi successivi al 1°
085400030715     C                   ELSE
085500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085600030715     C                   EVAL      posDa = posA + 1
085700030715     C* Eseguo lo scan x trovare la fine del campo corrente
085800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085900030715     C* Gestisco l'ultimo campo
086000030715     C                   IF        posA = *zeros
086100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C                   LEAVE
086600030715     C                   ELSE
086700030715     C* A questo punto "estraggo" il campo corrente
086800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086900030715     C                                                    (posA-posDa))
087000030715     C                   ENDIF
087100030715     C                   ENDIF
087200030715     C* Incremento il contatore d campo
087300030715     C                   EVAL      i = i +1
087400030715     C                   ENDDO
087500030715     C*
087600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087700030715     C* delimitazione testo
087800030715     C                   EVAL      i = 1
087900030715     C                   DOW       i < %elem(SkSplitFLD)
088000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088200030715     C                   EVAL      i = i + 1
088300030715     C                   ENDDO
088400030715     C                   EVAL      wGiro = 1
088500080716     C* Ripristino la 1° riga originale
088600080716     C                   EVAL      vinDTA = depvinDTA
088700030715     C                   ELSE
088800030715     C                   EVAL      wGiro = 2
088900031204     C**********
089000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089100031204     C* d separatore campo
089200031204     C**********
089300031204     C                   EVAL      vindta = %trim(vindta)
089400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089500031204     C                   EVAL      vindta = %subst(vindta:2)
089600031204     C                   ENDDO
089700031204     C*
089800031204     C                   Z-ADD     *zeros        lunghInput        4 0
089900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090300031204     C                   ENDDO
090400030715     C**********
090500030715     C* Effettuo lo split del campo dati d input
090600030715     C**********
090700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090800030715     C                   DOW       posDa <= %len(%trim(vindta))
090900030715     C*
091000030715     C* Gestisco il 1° campo
091100030715     C                   IF        i = 1
091200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091300030715     C                   EVAL      posDa = 1
091400030715     C* Eseguo lo scan x trovare la fine del primo campo
091500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091600030715     C* A questo punto "estraggo" il campo corrente
091700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091800030715     C                                                    (posA-posDa))
091900030715     C* X i campi successivi al 1°
092000030715     C                   ELSE
092100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092200030715     C                   EVAL      posDa = posA + 1
092300031204     C* Verifico che nn vi sia il campo nn valorizzato
092400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092500031204     C* Se campo nn valorizzato skippo al prossimo
092600031204     C                   EVAL      SkSplitCSV(i) = *blanks
092700031204     C                   EVAL      posA  = posA + 1
092800031204     C                   ELSE
092900030715     C* Eseguo lo scan x trovare la fine del campo corrente
093000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093100030715     C* Gestisco l'ultimo campo
093200030715     C                   IF        posA = *zeros
093300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093400030715     C* A questo punto "estraggo" il campo corrente
093500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093600030715     C                                                    (posA-posDa))
093700030715     C                   LEAVE
093800030715     C                   ELSE
093900030715     C* A questo punto "estraggo" il campo corrente
094000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094100030715     C                                                    (posA-posDa))
094200030715     C                   ENDIF
094300031204     C                   ENDIF
094400030715     C                   ENDIF
094500030715     C* Incremento il contatore d campo
094600030715     C                   EVAL      i = i +1
094700030715     C                   ENDDO
094800030715     C*
094900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095000030715     C* delimitazione testo
095100030715     C                   EVAL      i = 1
095200030715     C                   DOW       i < %elem(SkSplitCSV)
095300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095500030715     C                   EVAL      i = i + 1
095600030715     C                   ENDDO
095700030715     C*
095800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095900030715     C                   EVAL      i = 1
096000030715     C                   DOW       i < %elem(SkSplitFLD)
096100030715     C*
096200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096400030715     C                             SkSplitCSV(i) <> *blanks
096500030721     C*** ATB
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096700030721     C                   EVAL      VABATB = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030715     C*** RSD
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097400030715     C* ==
097500030715     C                   ENDIF
097600030721     C*** RD2
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030715     C*** IND
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098200030715     C                   EVAL      VABIND = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** LOD
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** PRD
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030721     C*** NZD
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** RMA
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** NOT
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** NT2
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** VCA
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** VAS
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** FFD
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030721     C*** GC1
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** GC2
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** TSP
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103100030721     C                   ENDIF
103200040510     C*** CBO
103300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103500040510     C                   ENDIF
103600030721     C*** NAS
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** TIC
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** GCA
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** XCO
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** CTM
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** TCR
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** CTS
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** FTM
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** VAD
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GMA
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** GGA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** GVA
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TC1
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** TC2
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** SCL
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** RMO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** NMO
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*
110500030721     C* Reperisco quindi i campi numerici...
110600040510     C*** NSP
110700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110800040510     C                   EVAL      PiStr=SkSplitCSV(i)
110900040510     C                   EXSR      CHKNUM
111000040510     C                   IF        PiInt=*on
111100040510     C                   Z-ADD     PiVal         VABNSP
111200040510     C                   ELSE
111300040510     C                   SETON                                        31
111400040510     C                   EVAL      VABNSP = *zeros
111500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111600040510     C                             + ' ' + 'VABNSP'
111700040510     C                   ENDIF
111800040510     C                   ENDIF
111900071106     C*** RMN
112000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112100071106     C                   EVAL      PiStr=SkSplitCSV(i)
112200071106     C                   EXSR      CHKNUM
112300071106     C                   IF        PiInt=*on
112400071106     C                   Z-ADD     PiVal         VABRMN
112500071106     C                   ELSE
112600071106     C                   SETON                                        32
112700071106     C                   EVAL      VABRMN = *zeros
112800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112900071106     C                             + ' ' + 'VABRMN'
113000071106     C                   ENDIF
113100071106     C                   ENDIF
113200030721     C*** CAD
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on
113700030721     C                   Z-ADD     PiVal         Num5_0
113800030721     C                   MOVEL(p)  Num5_0        VABCAD
113900030721     C                   ELSE
114000090603     C                   EVAL      VABCAD = SkSplitCSV(i)
114100090603     C                   SETON                                        32
114200090603     C                   EVAL      VABCAD = *zeros
114300090603     C                   EVAL      vinmsg = %trimr(vinmsg)
114400090603     C                             + ' ' + 'VABCAD'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** NCL
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on
115200030721     C                   Z-ADD     PiVal         VABNCL
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABNCL = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700040122     C                             + ' ' + 'VABNCL'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** PKB
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiNum=*on
116500030721     C                   Z-ADD     PiVal         VABPKB
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABPKB = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABPKB'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** CAS
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117500080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
117600080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
117700081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
117800081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
117900080125     C                   EVAL      wFlgCAS = '1'
118000080125     C                   ENDIF
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiNum=*on
118400030721     C                   Z-ADD     PiVal         VABCAS
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABCAS = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900030721     C                             + ' ' + 'VABCAS'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** CCM
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiInt=*on      AND
119700030721     C                             PiVal<=9999999 AND
119800030721     C                             PiVal>*zeros
119900030721     C                   Z-ADD     PiVal         VABCCM
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      VABCCM = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400040510     C                             + ' ' + 'VABCCM'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** LNP
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on    AND
121200030721     C                             PiVal<=999   AND
121300030721     C                             PiVal>*zeros
121400030721     C                   Z-ADD     PiVal         VABLNP
121500030721     C                   ELSE
121600040715     C                   SETON                                        32
121700030721     C                   EVAL      VABLNP = *zeros
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900040510     C                             + ' ' + 'VABLNP'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** NRS
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on AND
122700030721     C                             PiVal<=99
122800030721     C                   Z-ADD     PiVal         VABNRS
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABNRS'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** LNA
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on    AND
124000030721     C                             PiVal<=999   AND
124100030721     C                             PiVal>*zeros
124200030721     C                   Z-ADD     PiVal         VABLNA
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      VABLNA = *zeros
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABLNA'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** CTR
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on  AND
125500030721     C                             PiVal<=999
125600030721     C                   Z-ADD     PiVal         VABCTR
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABCTR'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** IAS
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiNum=*on
126800030721     C                   Z-ADD     PiVal         VABIAS
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABIAS'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** VLB
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiNum=*on
128000030721     C                   Z-ADD     PiVal         VABVLB
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABVLB'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** QFT
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiNum=*on
129200030721     C                   Z-ADD     PiVal         VABQFT
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABQFT'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** NCD
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on      AND
130400030721     C                             PiVal<=9999999
130500030721     C                   Z-ADD     PiVal         VABNCD
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABNCD'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** NCA
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiInt=*on      AND
131700030721     C                             PiVal<=9999999
131800030721     C                   Z-ADD     PiVal         VABNCA
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABNCA'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** ZNC
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on AND
133000030721     C                             PiVal<=99
133100030721     C                   Z-ADD     PiVal         VABZNC
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABZNC'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** DCR
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on       AND
134300030721     C                             PiVal<=99999999
134400030721     C                   Z-ADD     PiVal         VABDCR
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABDCR'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** HCR
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on   AND
135600030721     C                             PiVal<=9999
135700030721     C                   Z-ADD     PiVal         VABHCR
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABHCR'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** VMD
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiNum=*on
136900030721     C                   Z-ADD     PiVal         VABVMD
137000030721     C                   ELSE
137100030721     C                   SETON                                        32
137200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137300030721     C                             + ' ' + 'VABVMD'
137400030721     C                   ENDIF
137500030721     C                   ENDIF
137600030721     C*** ANT
137700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137800030721     C                   EVAL      PiStr=SkSplitCSV(i)
137900030721     C                   EXSR      CHKNUM
138000030721     C                   IF        PiInt=*on        AND
138100030721     C                             PiVal<=999999999
138200030721     C                   Z-ADD     PiVal         VABANT
138300030721     C                   ELSE
138400030721     C                   SETON                                        32
138500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138600030721     C                             + ' ' + 'VABANT'
138700030721     C                   ENDIF
138800030721     C                   ENDIF
138900030721     C*** CMO
139000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139100030721     C                   EVAL      PiStr=SkSplitCSV(i)
139200030721     C                   EXSR      CHKNUM
139300030721     C                   IF        PiInt=*on
139400030721     C                   Z-ADD     PiVal         Num5_0
139500030721     C                   MOVEL(p)  Num5_0        VABCMO
139600030721     C                   ELSE
139700030721     C                   SETON                                        32
139800030721     C                   EVAL      VABCMO = *zeros
139900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140000030721     C                             + ' ' + 'VABCMO'
140100030721     C                   ENDIF
140200030721     C                   ENDIF
140300040510     C*
140400040510     C* Reperisco i campi del FIVAT
140500040510     C*** VATNOT
140600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140800040510     C                   ENDIF
140900040510     C*** VATTRC
141000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141200040510     C                   ENDIF
141300060331     C*** VATNOT_A
141400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141600060331     C                   ENDIF
141700060331     C*** VATNOT_B
141800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142000060331     C                   ENDIF
142100060331     C*** VATNOT_E
142200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142400060331     C                   ENDIF
142500070823     C*
142600071029     C***  ===>  Gestione campi "particolari" dentro ciclo
142700090421     C*
142800090921     C*** W_VABRSD
142900090622     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
143000090622     C                   MOVEL     SkSplitCSV(i) wRSD
143100090622     C                   EVAL      VABRSD = %subst(wRSD:1:35)
143200090622     C                   EVAL      VABRD2 = %subst(wRSD:36:35)
143300090622     C                   ENDIF
143400100112     C*** W_VATNOTA1
143500100112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTA1'
143600100112     C                   EVAL      wVATNOT_A = %trim(wVATNOT_A) + ' ' +
143700090921     C                                         %trim(SkSplitCSV(i))
143800090921     C                   ENDIF
143900100112     C*** W_VATNOTB1
144000100112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTB1'
144100090921     C                   EVAL      wVATNOT_B = %trim(wVATNOT_B) + ' ' +
144200090921     C                                         %trim(SkSplitCSV(i))
144300090921     C                   ENDIF
144400070925     C*
144500071029     C***  <===  -----------------------------------------
144600040510     C*
144700030715     C                   ENDIF
144800030715     C                   EVAL      i = i + 1
144900030715     C                   ENDDO
145000070925     C*
145100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145200090921     C*
145300090921     C*
145400090921     C* NSP => Stacco un numeratore da AZNUM
145500090921     C                   clear                   TRUL33DS
145600090921     C                   eval      I33OPE = *zeros
145700090921     C                   eval      I33CNU = 302
145800090921     C                   eval      I33NUM = 1
145900090921     C                   movel     TRUL33DS      KPJBU
146000090921     C                   call      'TRUL33R'
146100090921     C                   parm                    KPJBA
146200090921     C                   movel     KPJBU         TRUL33DS
146300090921     C                   if        O33ERR = *zeros
146400090921     C                   z-add     O33NRF        VABNSP
146500090921     C                   z-add     O33NRF        VATNSP
146600090921     C                   else
146700090921     C                   SETON                                        31
146800090921     C                   EVAL      vinmsg = %trimr(vinmsg)
146900090921     C                             + ' ' + 'VABNSP VATNSP'
147000090921     C                   endif
147100080125     C*
147200070925     C*
147300071029     C***  <===  -----------------------------------------
147400070925     C*
147500070925     C* Considerazioni finali su CBO/CAS
147600080125     C                   IF        wFlgCAS = '1'
147700070925     C                   IF        VABCBO = '1'
147800070925     C                   EVAL      VABCBO = '4'
147900070925     C                   ENDIF
148000070925     C                   IF        VABCBO = '2'
148100070925     C                   EVAL      VABCBO = '6'
148200070925     C                   ENDIF
148300070925     C                   ENDIF
148400070925     C***  <===  ----------------------------
148500040510     C*
148600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148700040510     C                   IF        VABNSP = *zeros
148800040510     C                   SETON                                        31
148900040510     C                   ENDIF
149000040715     C*
149100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149200040715     C                   IF        VABLNP = *zeros
149300040715     C                   SETON                                        31
149400040715     C                   ENDIF
149500040510     C*
149600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149700040510     C                   IF        VABRMN = *zeros
149800040510     C                   Z-ADD     VABNSP        VABRMN
149900040510     C                   ENDIF
150000040510     C*
150100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150200040510     C                   IF        VABRMA = *blanks
150300040714     C                   MOVEL     VABRMN        VABRMA
150400040510     C                   ENDIF
150500040128     C*
150600040510     C* Se provincia nn valorizzata la reperisco
150700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150800040510     C                   IF        VABPRD = *blanks
150900040128     C                   CLEAR                   TISI95DS
151000040128     C                   EVAL      I95TCN = '3'
151100040128     C                   Z-ADD     datcor        I95DAT
151200040128     C                   EVAL      I95NAR = VABNZD
151300040128     C                   EVAL      I95CAP = VABCAD
151400040128     C                   EVAL      I95LOC = VABLOD
151500040128     C                   CALL      'TISI95R'
151600040128     C                   PARM                    TISI95DS
151700040128     C                   EVAL      VABPRD = O95PRV
151800040510     C                   ENDIF
151900020204     C*
152000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152100020204     C                   EXSR      CHKIMPDIV
152200030715     C                   ENDIF
152300020204     C*
152400010607     C                   ENDSR
152500010607     C*----------------------------------------------------*
152600010601
152700020204
152800020204     C*----------------------------------------------------*
152900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153000020204     C*----------------------------------------------------*
153100020204     C     CHKIMPDIV     BEGSR
153200020204     C*
153300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153400020204     C                   Z-ADD     *zeros        wrkDec            9 9
153500020204     C*
153600020204     C* Come prima cosa effettuo considerazioni sulla divisa
153700020204     C                   IF        vabIAS > *zeros
153800020204     C                   IF        vabVAS <> 'EUR'
153900020204     C                   EVAL      vabVAS =  'ITL'
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C                   IF        vabCAS > *zeros
154400020204     C                   IF        vabVCA <> 'EUR'
154500020204     C                   EVAL      vabVCA =  'ITL'
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C                   IF        vabVMD > *zeros
155000020204     C                   IF        vabVAD <> 'EUR'
155100020204     C                   EVAL      vabVAD =  'ITL'
155200020204     C                   ENDIF
155300020204     C                   ENDIF
155400020204     C*
155500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155600020204     C                   Z-ADD     vabIAS        wrkDec
155700020204     C                   IF        wrkDec > *zeros
155800020204     C                   IF        vabVAS = 'ITL'
155900020204     C                   EVAL      vabIAS = *zeros
156000020204     C                   ENDIF
156100020204     C                   ENDIF
156200020204     C*
156300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156400020204     C                   Z-ADD     vabCAS        wrkDec
156500020204     C                   IF        wrkDec > *zeros
156600020204     C                   IF        vabVCA = 'ITL'
156700020204     C                   EVAL      vabCAS = *zeros
156800020204     C                   ENDIF
156900020204     C                   ENDIF
157000020204     C*
157100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157200020204     C                   Z-ADD     vabVMD        wrkDec
157300020204     C                   IF        wrkDec > *zeros
157400020204     C                   IF        vabVAD = 'ITL'
157500020204     C                   EVAL      vabVMD = *zeros
157600020204     C                   ENDIF
157700020204     C                   ENDIF
157800020204     C*
157900020204     C                   ENDSR
158000020204     C***
158100080617
158200080617
158300080617
158400080617     C*----------------------------------------------------*
158500080617     C*  CONTROLLO NUMERICITA' CAMPI
158600080617     C*----------------------------------------------------*
158700080617     C     CHKNUM        BEGSR
158800080617     C*
158900080617     C                   IF        PiDecChr = *blanks
159000080617     C                   EVAL      PiDecChr = CharNUM
159100080617     C                   ENDIF
159200080617     C*
159300080617     C                   callp     UBISNUM_Check(PiStr
159400080617     C                                          :PiDecChr
159500080617     C                                          :PiVal
159600080617     C                                          :PiNum
159700080617     C                                          :PiInt)
159800080617     C*
159900080617     C                   ENDSR
160000080617     C***
160100010330
160200010601
160300010601
160400010601
160500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160600010601     C     repfil        BEGSR
160700010601     C*
160800010601     C                   if        invfil = *zeros and
160900010601     C                             depfil > *zeros and
161000010629     C                             (vinflg = *blanks or
161100010629     C                              vinflg = *zeros)
161200010601     C
161300010601     C                   eval      invfil = depfil
161400010601     C                   endif
161500010601     C*
161600010601     C                   if        depfil <> invfil and
161700010601     C                             invfil > *zeros
161800010601     C                   eval      flgMulti = '1'
161900010601     C                   if        vinflg = *blanks
162000010601     C                   add       1             cntNonEl
162100010601     C                   endif
162200010601     C                   endif
162300010601     C*
162400010601     C                   if        vinflg = '2'
162500010601     C                   eval      flgStato = '2'
162600010601     C                   endif
162700010601     C*
162800010601     C                   ENDSR
162900010601     C***
163000010601
163100010601
163200010601
163300010330
163400040119      /TITLE Invio dei dati al punto operativo.
163500040119     C     invio         BEGSR
163600040119     C*
163700040119     C* 1° invio FIVAT
163800040119     C                   reset                   dscmz
163900040119     C                   move      vlrpoi        cmzdst
164000040119     C                   eval      cmzfld = 'FIVATWWR'
164100040119     C                   eval      cmzmbd = vlrhdl
164200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164300040119     C***                if        prmfir = *blanks
164400040119     C                   eval      cmzfla = 'FIVAT00F'
164500040119     C                   eval      cmzmba = 'FIVAT00F'
164600040119     C***                else
164700040119     C***                eval      cmzfla = prmfir
164800040119     C***                eval      cmzmba = prmfir
164900040119     C***                endif
165000040119     C                   eval      cmznrr = *zeros
165100040119     C                   move      §ctrokvt      cmznrr
165200040119     C                   eval      cmzlba = vlrfl1
165300040119     C                   call(e)   'TIS711C'
165400040119     C                   parm                    dscmz
165500040119     C                   parm      *blanks       esito
165600040119     C                   if        %error
165700040119     C                             or cmzerr = '1'
165800040119     C                             or esito  = '1'
165900040119     C                   eval      wrkesito = '3'
166000040119     C                   else
166100040119     C*
166200040119     C* 2° invio FIVAB
166300040119     C                   reset                   dscmz
166400040119     C                   move      vlrpoi        cmzdst
166500040119     C                   eval      cmzfld = vlrfou
166600040119     C                   eval      cmzmbd = vlrhdl
166700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166800040119     C***                if        prmfir = *blanks
166900040119     C                   eval      cmzfla = 'FIVAB00F'
167000040119     C                   eval      cmzmba = 'FIVAB00F'
167100040119     C***                else
167200040119     C***                eval      cmzfla = prmfir
167300040119     C***                eval      cmzmba = prmfir
167400040119     C***                endif
167500040119     C                   eval      cmznrr = *zeros
167600040119     C                   move      §ctrokvb      cmznrr
167700040119     C                   eval      cmzlba = vlrfl1
167800040119     C                   call(e)   'TIS711C'
167900040119     C                   parm                    dscmz
168000040119     C                   parm      *blanks       esito
168100040119     C                   if        %error
168200040119     C                             or cmzerr = '1'
168300040119     C                             or esito  = '1'
168400040119     C                   eval      wrkesito = '3'
168500040119     C                   endif
168600040119     C                   endif
168700040119     C*
168800040119     C                   ENDSR
168900040119     C***
169000010601
169100010601
169200010601
169300010601
169400010601      /TITLE Invio dei dati al punto operativo.
169500010601     C     opeini        BEGSR
169600010601     C*
169700010601     C* Inizializzo flag e contatori operativi
169800010601     C                   movel     '0'           flgGiro           1
169900010601     C                   movel     '0'           flgMulti          1
170000010601     C                   movel     '1'           flgStato          1
170100010615     C                   movel     '0'           flgOk             1
170200010601     C                   z-add     *zeros        cntNonEl         10 0
170300010601     C                   z-add     *zeros        depfil            3 0
170400010601     C                   z-add     *zeros        invfil            3 0
170500010601     C*
170600010601     C                   ENDSR
170700010601     C***
170800070326
170900070326
171000070326
171100070326
171200080916     C     *pssr         BEGSR
171300070329     C*
171400080916     C                   if        %open(tivin00r)
171500080916     C                   close     tivin00r
171600080916     C                   endif
171700080916     C                   if        %open(fivabwwr)
171800080916     C                   close     fivabwwr
171900080916     C                   endif
172000080916     C                   if        %open(fivatwwr)
172100080916     C                   close     fivatwwr
172200080916     C                   endif
172300070326     C*
172400070326     C* Effettuo la chiamata al CLLE preposto
172500080916     C                   call(e)   'TITVVTC'
172600080916     C                   parm                    parccm
172700080916     C                   parm                    parmbr
172800080916     C                   parm      '2'           paropz
172900070326     C*
173000080916     C                   eval      wrkesito = '2'
173100070404     C*
173200080916     C                   seton                                        LR
173300070326     C*
173400080916     C                   ENDSR     '*CANCL'
173500070326     C***
173600070326
173700070326
173800010330
173900010330
174000000613     C     *inzsr        BEGSR
174100990910     C*
174200990910     C     *entry        plist
174300990920     C                   parm                    tivlrds
174400990921     C                   parm      wrkesito      esito
174500000724     C                   parm                    prmlit
174600000710     C                   parm                    prmfir
174700010330     C*
174800010330     C* CALCOLA LA DATA CORRENTE
174900010330     C                   time                    wn14             14 0
175000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
175100010330     C                   z-add     wn8           g08dat
175200010330     C                   z-add     *zeros        g08inv
175300010330     C                   movel     '0'           g08err
175400010330     C                   call      'XSRDA8'
175500010330     C                   parm                    wlbda8
175600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
175700000613     C*
175800000613     C                   ENDSR
175900000613     C***
176000990908
176100090921     Otitv1r8p  E            riepilogo         2
176200990915     O                                              'Upload via Internet'
176300990915     O                                           +1 'Traduzione TIVIN00R -'
176400021025     O                                           +1 'FIVABWWR'
176500990915     O                                           +1 'Testate bolle'
176600990915     O          E            riepilogo   2
176700990915     O                       wrkdata
176800990915     O                       wrkora              +1
176900990915     O                       procname            +1
177000990915     O          E            riepilogo   2
177100990915     O                                              'Cliente..................:'
177200010605     O                       VABCCM        z     +1
177300990915     O          E            riepilogo   2
177400990920     O                                              'Riferimento Strategi.....:'
177500990920     O                       vlrhdl              +1
177600990915     O          E            riepilogo   2
177700990915     O                                              'Giusti...................:'
177800040119     O                       §CTROKVB      2   +  1
177900990915     O          E            riepilogo   2
178000990915     O                                              'Sbagliati e corretti.....:'
178100971022     O                       §CTRMO        2   +  1
178200990915     O          E            riepilogo   2
178300990915     O                                              'Sbagliati e scartati.....:'
178400971022     O                       §CTRNO        2   +  1
178500000613
178600090921     Otitv1r8ps E            testdett          2
178700000613     O                                              'Upload via Internet'
178800000613     O                                           +1 'Traduzione TIVIN00R -'
178900021025     O                                           +1 'FIVABWWR'
179000000613     O                                           +1 'Testate bolle'
179100000616     O          E            testdett    3
179200000613     O                                           +2 'N° rec'
179300000613     O                                           +3 'Anteprima contenuto'
179400000616     O          E            rigadett    2
179500000613     O                       rrnum               +2
179600000621     O                       recko               +3
179700000616     O          E            findett     2
179800000613     O                       wrkdata
179900000613     O                       wrkora              +1
180000000613     O                       procname            +1
180100000616     O          E            findett     2
180200000613     O                                              'Cliente..................:'
180300010605     O                       VABCCM        z     +1
180400000616     O          E            findett     2
180500000613     O                                              'Riferimento Strategi.....:'
180600000613     O                       vlrhdl              +1
180700000616     O          E            findett     2
180800000613     O                                              'Giusti...................:'
180900040119     O                       §CTROKVB      2   +  1
181000000616     O          E            findett     2
181100000613     O                                              'Sbagliati e corretti.....:'
181200000613     O                       §CTRMO        2   +  1
181300000616     O          E            findett     2
181400000613     O                                              'Sbagliati e scartati.....:'
181500000613     O                       §CTRNO        2   +  1
181600000613** CMD - COMANDI CL
181700090921OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1R8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
181800090921OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1R8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
181900090921DLTOVR FILE(TITV1R8P TITV1R8PS) LVL(*)
182000010606
182100000613
182200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182300090921VABRMN;VABRMA;FIL001;W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNAS;
182400100112FIL002;W_VATNOTA1;W_VATNOTB1;VATNOT_E;
182500090921
