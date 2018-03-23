000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800091117     Ftitv1s8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100091117     Ftitv1s8ps O    f  198        PRINTER usropn
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
004700091117     D datcor          s              8  0 INZ
004800030715     D*------------------
004900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005000030715     D*------------------
005100030715     D SkSplitFLD      S             10    DIM(100)
005200030715     D SkSplitCSV      S            256    DIM(100)
005300030715     D CharCSV         S              1
005400030715     D CharTXT         S              1
005500030715     D CharNUM         S              1
005600030715     D posDa           S              3  0 INZ(*zeros)
005700030715     D posA            S              3  0 INZ(*zeros)
005800030715     D i               s              3  0 INZ(1)
005900030715     D wGiro           s              1  0 INZ(*zeros)
006000080125     D wVATNOT_A       s                   LIKE(VATNOT)
006100080125     D wVATNOT_B       s                   LIKE(VATNOT)
006200080125     D wVATNOT_E       s                   LIKE(VATNOT)
006300030822     D*------------------
006400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006500030822     D*------------------
006600030822     D posDaDft        S              3  0 INZ(*zeros)
006700030822     D posADft         S              3  0 INZ(*zeros)
006800030822     D j               s              3  0 INZ(1)
006900030822     D wGiroDft        s              1  0 INZ(*zeros)
007000031201     D*------------------
007100031201     D* Costanti
007200031201     D*------------------
007300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007500091117     D*------------------
007600091117
007700091117     D*------------------
007800091117     D* DS REPERIMENTO NUMERATORE
007900091117     D*------------------
008000091117     D trul33ds      e ds                  inz
008100091117     D*------------------
008200091117     D* DS ARCHITETTURA
008300091117     D*------------------
008400091117     D kpjba         e ds                  inz
008500091117
008600080617
008700080617     D*------------------
008800080617     D* LINKING A DEFINIZIONI ESTERNE
008900080617     D*------------------
009000080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009200010330
009300010330
009400010330
009500990915     C                   time                    wrkdata
009600990915     C                   time                    wrkora
009700000913     C                   reset                   rrnum
009800990921     C                   reset                   esito
009900081014     C                   reset                   wrkesito
010000010601     C*
010100010601     C                   exsr      opeini
010200010605     C                   exsr      rwvab
010300081014     C*
010400081014     C                   exsr      endela
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011200010601     C*--------------------------------------------------------
011300010601     C     PREELA        BEGSR
011400010601     C*
011500010601     C* SE OCCORRE SPEDIRE IN FILIALE
011600010601     C                   if        invfil <> *zeros and
011700010601     C                             flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100000724     C*
012200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012400010601     C     invfil        chain     azorg01l
012500000724     C                   if        %found
012600000724     C                   movel(p)  CMD(1)        depcmd
012700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012800000724     C*
012900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013000011113     C                   Z-ADD     150           LENGH            15 5
013100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200000724     C                   PARM                    depcmd
013300000724     C                   PARM                    LENGH
013400000724     C*
013500000724     C                   endif
013600000616     C*
013700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013800000616     C                   movel(p)  CMD(2)        depcmd
013900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014000000616     C*
014100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014200011113     C                   Z-ADD     150           LENGH            15 5
014300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014400000616     C                   PARM                    depcmd
014500000616     C                   PARM                    LENGH
014600000613     C*
014700091117     C                   if        not %open(titv1s8ps)
014800091117     C                   open      titv1s8ps
014900070329     C                   except    testdett
015000000613     C                   endif
015100010601     C*
015200010601     C                   endif
015300010601     C*
015400010601     C                   ENDSR
015500010601     C***
015600010601
015700010601
015800010601
015900010601
016000010601     C*--------------------------------------------------------
016100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016200010601     C*--------------------------------------------------------
016300010601     C     ENDELA        BEGSR
016400081014     C*
016500081014     C                   if        %open(tivin00r)
016600081014     C                   close     tivin00r
016700081014     C                   endif
016800081014     C                   if        %open(fivabwwr)
016900081014     C                   close     fivabwwr
017000081014     C                   endif
017100081014     C                   if        %open(fivatwwr)
017200081014     C                   close     fivatwwr
017300081014     C                   endif
017400040119     C*
017500040119     C* Effettuo la chiamata al CLLE preposto
017600040119     C                   call(e)   'TITVVTC'
017700040510     C                   parm                    parccm
017800040119     C                   parm                    parmbr
017900040119     C                   parm      '2'           paropz
018000050201     C*
018100050201     C* Effettuo lancio TISI95 solo x chiusura
018200050201     C                   CLEAR                   TISI95DS
018300050201     C                   EVAL      I95TLA = 'C'
018400050201     C                   CALL      'TISI95R'
018500050201     C                   PARM                    TISI95DS
018600000613     C*
018700091117     C                   if        %open(titv1s8ps)
018800000616     C                   except    findett
018900091117     C                   close     titv1s8ps
019000000613     C                   endif
019100000616     C*
019200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019300011113     C                   Z-ADD     150           LENGH            15 5
019400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019500000616     C                   PARM                    CMD(3)
019600000616     C                   PARM                    LENGH
019700000616     C*
019800010601     C                   ENDSR
019900010601     C***
020000010601
020100010601
020200010601
020300000613
020400000613     C*--------------------------------------------------------
020500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020600000613     C*--------------------------------------------------------
020700000621     C     STPR          BEGSR
020800000613     C*
020900091117     C                   if        not %open(titv1s8p)
021000091117     C   50              open      titv1s8p
021100990915     C                   endif
021200990915     C*
021300030715     C   50              except    riepilogo
021400990915     C*
021500091117     C                   if        %open(titv1s8p)
021600091117     C   50              close     titv1s8p
021700990914     C                   endif
021800000613     C*
021900000613     C                   ENDSR
022000000613     C***
022100000613
022200010601
022300010601
022400010330     C*--------------------------------------------------------
022500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022600010330     C*--------------------------------------------------------
022700010605     C     RWVAB         BEGSR
022800010330     C*
022900010330     C                   if        not %open(tivin00r)
023000010330     C                   open      tivin00r
023100010330     C                   endif
023200021025     C                   if        not %open(fivabwwr)
023300021025     C                   open      fivabwwr
023400010330     C                   endif
023500040119     C*
023600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023700040119     C                   exsr      prevat
023800040119     C*
023900040119     C                   if        chkcall = '0'
024000040119     C*
024100040119     C                   if        not %open(fivatwwr)
024200040119     C                   open      fivatwwr
024300040119     C                   endif
024400010330     C*
024500040119     C                   clear                   §CTROKVB          7 0
024600040119     C                   clear                   §CTROKVT          7 0
024700040119     C                   clear                   §CTRMO            7 0
024800040119     C                   clear                   §CTRNO            7 0
024900070823     C*
025000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025200091117     C***                exsr      inzvar
025300091117     C***                exsr      impvab
025400010330     C*
025500030822     C                   do        *HIVAL
025600010330     C*
025700030822     C                   read      tivin00r                               70
025800010618     C*
025900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026000010618     C                   if        vinflg = '1'
026100010618     C                   eval      flgOk = '1'
026200010618     C                   endif
026300040510     C*
026400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026500040510     C                   z-add     *zeros        wLenVINDTA        4 0
026600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027000040510     C                   z-add     1             wFlgVINDTA
027100040510     C                   leave
027200040510     C                   endif
027300040510     C                   enddo
027400090420     C*
027500090420     C                   add       1             rrnum
027600010618     C*
027700040510     C                   if        vindta > *blanks AND
027800090420     C                             wFlgVINDTA = 1
027900010330     C*
028000010601     C                   if        *in70 = *off and
028100010330     C                             (vinflg = *blanks
028200010330     C                              or vinflg = '0'
028300010330     C                              or vinflg = '2')
028400010330     C*
028500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028600010711     C                   if        vinflg = *blanks or vinflg = '0'
028700010711     C                   clear                   vinmsg
028800010711     C                   endif
028900091117     C*
029000091117     C* Forzatura x distinzione delimitatore campo e delimitare decimale
029100091117     C                   dow       %scan('","':vindta) > *zeros
029200091117     C                   eval      vindta = %replace('";"':vindta:
029300091117     C                                         %scan('","':vindta))
029400091117     C                   enddo
029500091117     C*
029600091117     C                   exsr      repNSP
029700010605     C                   exsr      impvab
029800010601     C*
029900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030000010605     C                   eval      depfil = VABLNP
030100010601     C                   exsr      repfil
030200010601     C                   if        depfil = invfil
030300021025     C                   if        vlrpoi = 999
030400030822     C                   move(P)   invfil        VABFGS
030500021025     C                   else
030600030822     C                   move(P)   vlrpoi        VABFGS
030700021025     C                   endif
030800010601     C*
030900010601     C                   exsr      PREELA
031000010601     C*
031100010604     C* Ebbene...
031200010604     C*
031300040119     C  N31              add       1             §CTROKVB
031400040119     C   32              add       1             §CTRMO
031500040119     C   31              add       1             §CTRNO
031600030822     C                   if        wGiro = 2
031700040510     C* Scrivo sempre il VAT
031800040510     C  N31              exsr      wrivat
031900040510     C* Scrivo il VAB solo a rottura d spedizione
032000040119     C                   if        depspe =  *zeros OR
032100040119     C                             depspe <> vabNSP
032200040119     C  N31              exsr      wrivab
032300040119     C  N31              eval      depspe = vabNSP
032400040119     C                   endif
032500040119     C                   endif
032600010604     C*
032700010604     C                   if        *in31 = *off and
032800010604     C                             *in32 = *off
032900010604     C                   eval      vinflg = '1'
033000010604     C                   else
033100010604     C                   eval      recko = vindta
033200010604     C                   except    rigadett
033300010604     C                   eval      vinflg = '2'
033400010604     C                   endif
033500010604     C                   endif
033600010601     C*
033700010604     C                   endif
033800010604     C*
033900010330     C                   else
034000010330     C                   eval      vinflg = '1'
034100010330     C                   endif
034200010601     C*
034300010601     C  N70              update    tivin000
034400010330     C*
034500030822     C  N70              enddo
034600040119     C*
034700040119     C                   endif                                                  (endif - chkcall)
034800010601     C*
034900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035000010601     C                   if        cntNonEl = *zeros or
035100010601     C                             flgMulti = '0'
035200010330     C* Se non ci sono record con errori ...
035300010601     C                   if        §ctrno = 0 and
035400010604     C                             §ctrmo = 0 and
035500010601     C                             flgStato <> '2'
035600010330     C* ... restituisco esito OK.
035700010330     C                   eval      wrkesito = '0'
035800010330     C                   else
035900040119     C                   if        §ctrokvb > 0
036000010330     C                   eval      wrkesito = '1'
036100010330     C                   else
036200010615     C                   if        flgOk = '0'
036300010615     C                   eval      wrkesito = '2'
036400010615     C                   else
036500010615     C                   eval      wrkesito = '6'
036600010615     C                   endif
036700010330     C                   endif
036800010330     C                   endif
036900010601     C                   else
037000010601     C                   eval      wrkesito = '9'
037100010601     C                   endif
037200010330     C*
037300010330     C                   if        %open(tivin00r)
037400010330     C                   close     tivin00r
037500010330     C                   endif
037600021025     C                   if        %open(fivabwwr)
037700021025     C                   close     fivabwwr
037800010330     C                   endif
037900040512     C                   if        %open(fivatwwr)
038000040512     C                   close     fivatwwr
038100040512     C                   endif
038200010601     C*
038300010601     C                   if        vlrpoi <> 999
038400010601     C                   eval      invfil = vlrpoi
038500010601     C                   endif
038600010330     C*
038700040119     C                   if        §ctrokvb > 0
038800010601     C                             and invfil > *zeros
038900010330     C                   exsr      invio
039000010330     C                   endif
039100010601     C*
039200010618     C                   if        flgGiro = '1'
039300081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039400010618     C                   endif
039500010330     C*
039600010330     C                   ENDSR
039700010330     C***
039800040119
039900040119
040000040119
040100040119     C*----------------------------------------------------*
040200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040300040119     C*----------------------------------------------------*
040400040510     C     WRIVAB        BEGSR
040500040119     C*
040600060331     C* Gestisco l'eventuale rottura x numero spedizione
040700070823     C                   if        VABNSP <> depspe
040800070301     C                   if        wVATNOT_E <> *blanks
040900080125     C                   if        VABCTM = *blanks
041000080125     C                   eval      VABCTM = '7Q'
041100080125     C                   endif
041200070301     C                   endif
041300040119     C                   write     fivab000                                     => scarico il VAB
041400060331     C                   endif
041500040119     C*
041600040119     C                   ENDSR
041700040119
041800040119
041900040119
042000040119     C*----------------------------------------------------*
042100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042200040119     C*----------------------------------------------------*
042300040510     C     WRIVAT        BEGSR
042400060512     C*
042500060512     C* Inizializzo i campi chiave
042600060512     C                   eval      VATFGS = VABFGS
042700060512     C                   eval      VATAAS = VABAAS
042800060512     C                   eval      VATCCM = VABCCM
042900070823     C                   eval      VATNRS = VABNRS
043000060512     C                   eval      VATNSP = VABNSP
043100060512     C                   eval      VATLNP = VABLNP
043200060331     C*
043300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043400060331     C                   if        wVATNOT_A <> *blanks
043500060331     C                   eval      VATNOT = wVATNOT_A
043600060331     C                   eval      VATTRC = 'A'
043700060331     C                   write     FIVAT000
043800060331     C                   add       1             §CTROKVT
043900060331     C                   endif
044000060331     C*
044100060331     C                   if        wVATNOT_B <> *blanks
044200060331     C                   eval      VATNOT = wVATNOT_B
044300060331     C                   eval      VATTRC = 'B'
044400060331     C                   write     FIVAT000
044500060331     C                   add       1             §CTROKVT
044600060331     C                   endif
044700060331     C*
044800080625     C                   if        wVATNOT_E <> *blanks
044900060331     C                   eval      VATNOT = wVATNOT_E
045000060331     C                   eval      VATTRC = 'E'
045100080716     C                   write     FIVAT000
045200080716     C                   add       1             §CTROKVT
045300080625     C                   endif
045400040119     C*
045500040119     C                   ENDSR
045600040119
045700040119
045800040119
045900040119     C*----------------------------------------------------*
046000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046100040119     C*----------------------------------------------------*
046200040119     C     PREVAT        BEGSR
046300040119     C*
046400040119     C* Compongo il nome del membro da dare al FIVATWWR
046500040119     C                   eval      parmbr = vlrhdl
046600040119     C                   movel     'M'           parmbr
046700040510     C                   eval      parccm = vlrksc
046800040119     C                   eval      paropz = '1'
046900040119     C* Effettuo la chiamata al CLLE preposto
047000040119     C                   call(e)   'TITVVTC'
047100040119     C                   parm                    parccm
047200040119     C                   parm                    parmbr
047300040119     C                   parm                    paropz
047400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047500040119     C                   if        %error
047600040119     C                   movel     '1'           chkcall
047700040119     C                   else
047800040119     C                   movel     '0'           chkcall
047900040119     C                   endif
048000040119     C*
048100040119     C                   ENDSR
048200010601
048300010601
048400010601
048500010330     C*----------------------------------------------------*
048600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048700010330     C*----------------------------------------------------*
048800040119     C     INZVAR        BEGSR
048900010330     C*
049000030822     C* Inizializzo variabili di wrk
049100020204     C                   Z-ADD     *zeros        Num5_0            5 0
049200060331     C                   MOVEL     *blanks       wVATNOT_A
049300060331     C                   MOVEL     *blanks       wVATNOT_B
049400060331     C                   MOVEL     *blanks       wVATNOT_E
049500090220     C*
049600080617     C                   EVAL      wFlgCAS = *blanks
049700030822     C*
049800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049900030822     C                   CLEAR                   FIVAB000
050000040119     C                   CLEAR                   FIVAT000
050100030822     C                   CLEAR                   SkSplitCSV
050200030822     C                   EVAL      i = 1
050300030822     C                   EVAL      posDa = *zeros
050400030822     C                   EVAL      posA  = *zeros
050500030822     C*
050600010330     C                   ENDSR
050700010330     C*----------------------------------------------------*
050800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050900010330     C*----------------------------------------------------*
051000010330     C     DEFCAM        BEGSR
051100030822     C*
051200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051300030715     C* e delimitatore testo.
051400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051700030822     C*
051800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
052000030715     C                   SETON                                        50
052100030715     C                   ELSE
052200030715     C                   SETOFF                                       50
052300030715     C                   ENDIF
052400090421     C*
052500090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
052600090421     C                   EVAL      VABCBO = '1'
052700091117     C                   EVAL      VABVAS = 'EUR'
052800090421     C                   EVAL      VABLNP = vlrpoi
052900090421     C                   EVAL      VABNSP = rrnum
053000030822     C*
053100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053200030822     C                   EVAL      posDaDft = 1
053300030822     C                   EVAL      posADft  = 0
053400030822     C                   EVAL      wGiroDft = 0
053500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053600030822     C                             posDaDft > 0
053700030822     C*
053800030822     C* Gestisco il 1° giro
053900030822     C                   IF        wGiroDft = 0
054000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054200030822     C* Incremento il contatore dei "giri"
054300030822     C                   EVAL      wGiroDft = 1
054400030822     C                   ELSE
054500030822     C                   EVAL      posDaDft = posADft
054600030822     C                   ENDIF
054700030822     C* Eseguo lo scan x trovare la fine del campo corrente
054800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054900030822     C*
055000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055200030822     C                   IF        posDaDft > 0 AND
055300030822     C                             posADft  > 0
055400030822     C* NCL
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'NCL'
055900030822     C                   EVAL      PiStr=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   EXSR      CHKNUM
056300030822     C                   IF        PiInt=*on
056400030822     C                   Z-ADD     PiVal         VABNCL
056500030822     C                   ENDIF
056600030822     C                   ENDIF
056700030822     C* CCM
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'CCM'
057200030822     C                   EVAL      PiStr=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   EXSR      CHKNUM
057600030822     C                   IF        PiInt=*on
057700030822     C                   Z-ADD     PiVal         VABCCM
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* LNP
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'LNP'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000030822     C                   Z-ADD     PiVal         VABLNP
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* NRS
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'NRS'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABNRS
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* CTR
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'CTR'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABCTR
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* PKB
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'PKB'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiNum=*on
062900030822     C                   Z-ADD     PiVal         VABPKB
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* VLB
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'VLB'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiNum=*on
064200030822     C                   Z-ADD     PiVal         VABVLB
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* QFT
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'QFT'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiNum=*on
065500030822     C                   Z-ADD     PiVal         VABQFT
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* CBO
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'CBO'
066300030822     C                   EVAL      VABCBO=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* TSP
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'TSP'
067200030822     C                   EVAL      VABTSP=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* VAS
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VAS'
068100030822     C                   EVAL      VABVAS=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* VCA
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'VCA'
069000030822     C                   EVAL      VABVCA=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400070925     C* TIC
069500070925     C                   IF        %subst(
069600070925     C                             %subst(vlrppt:posDaDft+1:
069700070925     C                             posADft-posDaDft-1):1:3)
069800070925     C                             = 'TIC'
069900070925     C                   EVAL      VABTIC=%trim(%subst(
070000070925     C                             %subst(vlrppt:posDaDft+1:
070100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200070925     C                   ENDIF
070300030822     C* GCA
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'GCA'
070800030822     C                   EVAL      VABGCA=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* CTM
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'CTM'
071700030822     C                   EVAL      VABCTM=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* FFD
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'FFD'
072600030822     C                   EVAL      VABFFD=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* VAD
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'VAD'
073500030822     C                   EVAL      VABVAD=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GMA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GMA'
074400030822     C                   EVAL      VABGMA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* GGA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GGA'
075300030822     C                   EVAL      VABGGA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* GVA
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'GVA'
076200030822     C                   EVAL      VABGVA=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* TC1
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'TC1'
077100030822     C                   EVAL      VABTC1=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* TC2
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'TC2'
078000030822     C                   EVAL      VABTC2=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400040714     C* VATTRC
078500040714     C                   IF        %subst(
078600040714     C                             %subst(vlrppt:posDaDft+1:
078700040714     C                             posADft-posDaDft-1):1:3)
078800040714     C                             = 'TRC'
078900040714     C                   EVAL      VATTRC=%trim(%subst(
079000040714     C                             %subst(vlrppt:posDaDft+1:
079100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200040714     C                   ENDIF
079300030822     C* ...
079400030822     C                   ENDIF
079500030822     C                   ENDDO
079600020204     C*
079700010330     C                   ENDSR
079800010607     C*----------------------------------------------------*
079900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080000010607     C*----------------------------------------------------*
080100010607     C     IMPVAB        BEGSR
080200010607     C*
080300010607     C                   SETOFF                                       3132
080400030515     C*
080500010607     C                   MOVEL     datcor        VABAAS
080600010607     C                   MOVE      datcor        VABMGS
080700010607     C*
080800030715     C**********
080900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081000030715     C**********
081100030715     C                   IF        wGiro = *zeros
081200080716     C*
081300080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
081400080716     C                   EVAL      depvinDTA = vinDTA
081500040113     C*
081600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081800040510     C                             %trim(intcol(3))<>*blanks
081900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082000040113     C                                      %trim(intcol(3))
082100070823     C     ';':CharCSV   XLATE     vindta        vindta
082200040510     C                   ENDIF
082300080626     C*
082400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082500031201     C     minu:maiu     XLATE     vindta        vindta
082600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082700030715     C                   DOW       posDa <= %len(%trim(vindta))
082800030715     C*
082900030715     C* Gestisco il 1° campo
083000030715     C                   IF        i = 1
083100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083200030715     C                   EVAL      posDa = 1
083300030715     C* Eseguo lo scan x trovare la fine del primo campo
083400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083500030715     C* A questo punto "estraggo" il campo corrente
083600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083700030715     C                                                    (posA-posDa))
083800030715     C* X i campi successivi al 1°
083900030715     C                   ELSE
084000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084100030715     C                   EVAL      posDa = posA + 1
084200030715     C* Eseguo lo scan x trovare la fine del campo corrente
084300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084400030715     C* Gestisco l'ultimo campo
084500030715     C                   IF        posA = *zeros
084600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084700030715     C* A questo punto "estraggo" il campo corrente
084800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084900030715     C                                                    (posA-posDa))
085000030715     C                   LEAVE
085100030715     C                   ELSE
085200030715     C* A questo punto "estraggo" il campo corrente
085300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085400030715     C                                                    (posA-posDa))
085500030715     C                   ENDIF
085600030715     C                   ENDIF
085700030715     C* Incremento il contatore d campo
085800030715     C                   EVAL      i = i +1
085900030715     C                   ENDDO
086000030715     C*
086100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086200030715     C* delimitazione testo
086300030715     C                   EVAL      i = 1
086400030715     C                   DOW       i < %elem(SkSplitFLD)
086500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086700030715     C                   EVAL      i = i + 1
086800030715     C                   ENDDO
086900030715     C                   EVAL      wGiro = 1
087000080716     C* Ripristino la 1° riga originale
087100080716     C                   EVAL      vinDTA = depvinDTA
087200030715     C                   ELSE
087300030715     C                   EVAL      wGiro = 2
087400031204     C**********
087500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087600031204     C* d separatore campo
087700031204     C**********
087800031204     C                   EVAL      vindta = %trim(vindta)
087900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088000031204     C                   EVAL      vindta = %subst(vindta:2)
088100031204     C                   ENDDO
088200031204     C*
088300031204     C                   Z-ADD     *zeros        lunghInput        4 0
088400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088800031204     C                   ENDDO
088900030715     C**********
089000030715     C* Effettuo lo split del campo dati d input
089100030715     C**********
089200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089300030715     C                   DOW       posDa <= %len(%trim(vindta))
089400030715     C*
089500030715     C* Gestisco il 1° campo
089600030715     C                   IF        i = 1
089700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089800030715     C                   EVAL      posDa = 1
089900030715     C* Eseguo lo scan x trovare la fine del primo campo
090000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090100030715     C* A questo punto "estraggo" il campo corrente
090200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090300030715     C                                                    (posA-posDa))
090400030715     C* X i campi successivi al 1°
090500030715     C                   ELSE
090600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090700030715     C                   EVAL      posDa = posA + 1
090800031204     C* Verifico che nn vi sia il campo nn valorizzato
090900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091000031204     C* Se campo nn valorizzato skippo al prossimo
091100031204     C                   EVAL      SkSplitCSV(i) = *blanks
091200031204     C                   EVAL      posA  = posA + 1
091300031204     C                   ELSE
091400030715     C* Eseguo lo scan x trovare la fine del campo corrente
091500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091600030715     C* Gestisco l'ultimo campo
091700030715     C                   IF        posA = *zeros
091800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C                   LEAVE
092300030715     C                   ELSE
092400030715     C* A questo punto "estraggo" il campo corrente
092500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092600030715     C                                                    (posA-posDa))
092700030715     C                   ENDIF
092800031204     C                   ENDIF
092900030715     C                   ENDIF
093000030715     C* Incremento il contatore d campo
093100030715     C                   EVAL      i = i +1
093200030715     C                   ENDDO
093300030715     C*
093400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093500030715     C* delimitazione testo
093600030715     C                   EVAL      i = 1
093700030715     C                   DOW       i < %elem(SkSplitCSV)
093800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094000030715     C                   EVAL      i = i + 1
094100030715     C                   ENDDO
094200030715     C*
094300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094400030715     C                   EVAL      i = 1
094500030715     C                   DOW       i < %elem(SkSplitFLD)
094600030715     C*
094700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094900030715     C                             SkSplitCSV(i) <> *blanks
095000030721     C*** ATB
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095200030721     C                   EVAL      VABATB = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030715     C*** RSD
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095900030715     C* ==
096000030715     C                   ENDIF
096100030721     C*** RD2
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030715     C*** IND
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096700030715     C                   EVAL      VABIND = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030715     C*** LOD
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** PRD
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030721     C*** NZD
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030715     C*** RMA
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030715     C*** NOT
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** NT2
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** VCA
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** VAS
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030715     C*** FFD
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030721     C*** GC1
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** GC2
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** TSP
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101600030721     C                   ENDIF
101700040510     C*** CBO
101800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102000040510     C                   ENDIF
102100030721     C*** NAS
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** TIC
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** GCA
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** XCO
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** CTM
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** TCR
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** CTS
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** FTM
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** VAD
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** GMA
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** GGA
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** GVA
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** TC1
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TC2
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** SCL
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** RMO
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** NMO
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*
109000030721     C* Reperisco quindi i campi numerici...
109100040510     C*** NSP
109200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109300040510     C                   EVAL      PiStr=SkSplitCSV(i)
109400040510     C                   EXSR      CHKNUM
109500040510     C                   IF        PiInt=*on
109600040510     C                   Z-ADD     PiVal         VABNSP
109700040510     C                   ELSE
109800040510     C                   SETON                                        31
109900040510     C                   EVAL      VABNSP = *zeros
110000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110100040510     C                             + ' ' + 'VABNSP'
110200040510     C                   ENDIF
110300040510     C                   ENDIF
110400071106     C*** RMN
110500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110600071106     C                   EVAL      PiStr=SkSplitCSV(i)
110700071106     C                   EXSR      CHKNUM
110800071106     C                   IF        PiInt=*on
110900071106     C                   Z-ADD     PiVal         VABRMN
111000071106     C                   ELSE
111100071106     C                   SETON                                        32
111200071106     C                   EVAL      VABRMN = *zeros
111300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111400071106     C                             + ' ' + 'VABRMN'
111500071106     C                   ENDIF
111600071106     C                   ENDIF
111700030721     C*** CAD
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on
112200030721     C                   Z-ADD     PiVal         Num5_0
112300030721     C                   MOVEL(p)  Num5_0        VABCAD
112400030721     C                   ELSE
112500090603     C                   EVAL      VABCAD = SkSplitCSV(i)
112600090603     C                   SETON                                        32
112700090603     C                   EVAL      VABCAD = *zeros
112800090603     C                   EVAL      vinmsg = %trimr(vinmsg)
112900090603     C                             + ' ' + 'VABCAD'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** NCL
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on
113700030721     C                   Z-ADD     PiVal         VABNCL
113800030721     C                   ELSE
113900030721     C                   SETON                                        32
114000030721     C                   EVAL      VABNCL = *zeros
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200040122     C                             + ' ' + 'VABNCL'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** PKB
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiNum=*on
115000030721     C                   Z-ADD     PiVal         VABPKB
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABPKB = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABPKB'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** CAS
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116000080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
116100080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
116200081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
116300081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
116400080125     C                   EVAL      wFlgCAS = '1'
116500080125     C                   ENDIF
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiNum=*on
116900030721     C                   Z-ADD     PiVal         VABCAS
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABCAS = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABCAS'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** CCM
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on      AND
118200030721     C                             PiVal<=9999999 AND
118300030721     C                             PiVal>*zeros
118400030721     C                   Z-ADD     PiVal         VABCCM
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABCCM = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900040510     C                             + ' ' + 'VABCCM'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** LNP
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiInt=*on    AND
119700030721     C                             PiVal<=999   AND
119800030721     C                             PiVal>*zeros
119900030721     C                   Z-ADD     PiVal         VABLNP
120000030721     C                   ELSE
120100040715     C                   SETON                                        32
120200030721     C                   EVAL      VABLNP = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400040510     C                             + ' ' + 'VABLNP'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** NRS
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on AND
121200030721     C                             PiVal<=99
121300030721     C                   Z-ADD     PiVal         VABNRS
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABNRS'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** LNA
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on    AND
122500030721     C                             PiVal<=999   AND
122600030721     C                             PiVal>*zeros
122700030721     C                   Z-ADD     PiVal         VABLNA
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      VABLNA = *zeros
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABLNA'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** CTR
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on  AND
124000030721     C                             PiVal<=999
124100030721     C                   Z-ADD     PiVal         VABCTR
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABCTR'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** IAS
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiNum=*on
125300030721     C                   Z-ADD     PiVal         VABIAS
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABIAS'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** VLB
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiNum=*on
126500030721     C                   Z-ADD     PiVal         VABVLB
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABVLB'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** QFT
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABQFT
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABQFT'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** NCD
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on      AND
128900030721     C                             PiVal<=9999999
129000030721     C                   Z-ADD     PiVal         VABNCD
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABNCD'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** NCA
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on      AND
130200030721     C                             PiVal<=9999999
130300030721     C                   Z-ADD     PiVal         VABNCA
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABNCA'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** ZNC
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on AND
131500030721     C                             PiVal<=99
131600030721     C                   Z-ADD     PiVal         VABZNC
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABZNC'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** DCR
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on       AND
132800030721     C                             PiVal<=99999999
132900030721     C                   Z-ADD     PiVal         VABDCR
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABDCR'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** HCR
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on   AND
134100030721     C                             PiVal<=9999
134200030721     C                   Z-ADD     PiVal         VABHCR
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABHCR'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** VMD
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiNum=*on
135400030721     C                   Z-ADD     PiVal         VABVMD
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABVMD'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** ANT
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on        AND
136600030721     C                             PiVal<=999999999
136700030721     C                   Z-ADD     PiVal         VABANT
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABANT'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** CMO
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiInt=*on
137900030721     C                   Z-ADD     PiVal         Num5_0
138000030721     C                   MOVEL(p)  Num5_0        VABCMO
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      VABCMO = *zeros
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABCMO'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800040510     C*
138900040510     C* Reperisco i campi del FIVAT
139000040510     C*** VATNOT
139100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139300040510     C                   ENDIF
139400040510     C*** VATTRC
139500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139700040510     C                   ENDIF
139800060331     C*** VATNOT_A
139900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140100060331     C                   ENDIF
140200060331     C*** VATNOT_B
140300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140500060331     C                   ENDIF
140600060331     C*** VATNOT_E
140700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140900060331     C                   ENDIF
141000070823     C*
141100071029     C***  ===>  Gestione campi "particolari" dentro ciclo
141200090421     C*
141300091117     C*** W_NOTE
141400091117     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOTE'
141500091117     C                   MOVEL     *blanks       wNOTE            70
141600091117     C                   MOVEL     SkSplitCSV(i) wNOTE
141700091117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
141800091117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
141900090622     C                   ENDIF
142000091117     C*** W_VABNZD
142100091117     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
142200091117     C                   IF        SkSplitCSV(i) = 'IT'  OR
142300091117     C                             SkSplitCSV(i) = 'ITA'
142400091117     C                   EVAL      VABNZD = *blanks
142500091117     C                   ENDIF
142600091117     C                   ENDIF
142700091117     C*** W_VABTSP
142800091117     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTSP'
142900091117     C                   IF        SkSplitCSV(i) = 'E'
143000091117     C                   EVAL      VABTSP = 'E'
143100091117     C                   ENDIF
143200091117     C                   ENDIF
143300070925     C*
143400071029     C***  <===  -----------------------------------------
143500040510     C*
143600030715     C                   ENDIF
143700030715     C                   EVAL      i = i + 1
143800030715     C                   ENDDO
143900070925     C*
144000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144100080125     C*
144200070925     C*
144300071029     C***  <===  -----------------------------------------
144400070925     C*
144500070925     C* Considerazioni finali su CBO/CAS
144600080125     C                   IF        wFlgCAS = '1'
144700070925     C                   IF        VABCBO = '1'
144800070925     C                   EVAL      VABCBO = '4'
144900070925     C                   ENDIF
145000070925     C                   IF        VABCBO = '2'
145100070925     C                   EVAL      VABCBO = '6'
145200070925     C                   ENDIF
145300070925     C                   ENDIF
145400070925     C***  <===  ----------------------------
145500040510     C*
145600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145700040510     C                   IF        VABNSP = *zeros
145800040510     C                   SETON                                        31
145900040510     C                   ENDIF
146000040715     C*
146100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146200040715     C                   IF        VABLNP = *zeros
146300040715     C                   SETON                                        31
146400040715     C                   ENDIF
146500040510     C*
146600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146700040510     C                   IF        VABRMN = *zeros
146800040510     C                   Z-ADD     VABNSP        VABRMN
146900040510     C                   ENDIF
147000040510     C*
147100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147200040510     C                   IF        VABRMA = *blanks
147300040714     C                   MOVEL     VABRMN        VABRMA
147400040510     C                   ENDIF
147500040128     C*
147600040510     C* Se provincia nn valorizzata la reperisco
147700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
147800040510     C                   IF        VABPRD = *blanks
147900040128     C                   CLEAR                   TISI95DS
148000040128     C                   EVAL      I95TCN = '3'
148100040128     C                   Z-ADD     datcor        I95DAT
148200040128     C                   EVAL      I95NAR = VABNZD
148300040128     C                   EVAL      I95CAP = VABCAD
148400040128     C                   EVAL      I95LOC = VABLOD
148500040128     C                   CALL      'TISI95R'
148600040128     C                   PARM                    TISI95DS
148700040128     C                   EVAL      VABPRD = O95PRV
148800040510     C                   ENDIF
148900020204     C*
149000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149100020204     C                   EXSR      CHKIMPDIV
149200030715     C                   ENDIF
149300020204     C*
149400010607     C                   ENDSR
149500010607     C*----------------------------------------------------*
149600010601
149700020204
149800020204     C*----------------------------------------------------*
149900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150000020204     C*----------------------------------------------------*
150100020204     C     CHKIMPDIV     BEGSR
150200020204     C*
150300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150400020204     C                   Z-ADD     *zeros        wrkDec            9 9
150500020204     C*
150600020204     C*
150700020204     C                   ENDSR
150800020204     C***
150900080617
151000080617
151100080617
151200080617     C*----------------------------------------------------*
151300080617     C*  CONTROLLO NUMERICITA' CAMPI
151400080617     C*----------------------------------------------------*
151500080617     C     CHKNUM        BEGSR
151600080617     C*
151700080617     C                   IF        PiDecChr = *blanks
151800080617     C                   EVAL      PiDecChr = CharNUM
151900080617     C                   ENDIF
152000080617     C*
152100080617     C                   callp     UBISNUM_Check(PiStr
152200080617     C                                          :PiDecChr
152300080617     C                                          :PiVal
152400080617     C                                          :PiNum
152500080617     C                                          :PiInt)
152600080617     C*
152700080617     C                   ENDSR
152800080617     C***
152900010330
153000010601
153100010601
153200010601
153300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
153400010601     C     repfil        BEGSR
153500010601     C*
153600010601     C                   if        invfil = *zeros and
153700010601     C                             depfil > *zeros and
153800010629     C                             (vinflg = *blanks or
153900010629     C                              vinflg = *zeros)
154000010601     C
154100010601     C                   eval      invfil = depfil
154200010601     C                   endif
154300010601     C*
154400010601     C                   if        depfil <> invfil and
154500010601     C                             invfil > *zeros
154600010601     C                   eval      flgMulti = '1'
154700010601     C                   if        vinflg = *blanks
154800010601     C                   add       1             cntNonEl
154900010601     C                   endif
155000010601     C                   endif
155100010601     C*
155200010601     C                   if        vinflg = '2'
155300010601     C                   eval      flgStato = '2'
155400010601     C                   endif
155500010601     C*
155600010601     C                   ENDSR
155700010601     C***
155800010601
155900010601
156000010601
156100010330
156200040119      /TITLE Invio dei dati al punto operativo.
156300040119     C     invio         BEGSR
156400040119     C*
156500040119     C* 1° invio FIVAT
156600040119     C                   reset                   dscmz
156700040119     C                   move      vlrpoi        cmzdst
156800040119     C                   eval      cmzfld = 'FIVATWWR'
156900040119     C                   eval      cmzmbd = vlrhdl
157000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
157100040119     C***                if        prmfir = *blanks
157200040119     C                   eval      cmzfla = 'FIVAT00F'
157300040119     C                   eval      cmzmba = 'FIVAT00F'
157400040119     C***                else
157500040119     C***                eval      cmzfla = prmfir
157600040119     C***                eval      cmzmba = prmfir
157700040119     C***                endif
157800040119     C                   eval      cmznrr = *zeros
157900040119     C                   move      §ctrokvt      cmznrr
158000040119     C                   eval      cmzlba = vlrfl1
158100040119     C                   call(e)   'TIS711C'
158200040119     C                   parm                    dscmz
158300040119     C                   parm      *blanks       esito
158400040119     C                   if        %error
158500040119     C                             or cmzerr = '1'
158600040119     C                             or esito  = '1'
158700040119     C                   eval      wrkesito = '3'
158800040119     C                   else
158900040119     C*
159000040119     C* 2° invio FIVAB
159100040119     C                   reset                   dscmz
159200040119     C                   move      vlrpoi        cmzdst
159300040119     C                   eval      cmzfld = vlrfou
159400040119     C                   eval      cmzmbd = vlrhdl
159500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159600040119     C***                if        prmfir = *blanks
159700040119     C                   eval      cmzfla = 'FIVAB00F'
159800040119     C                   eval      cmzmba = 'FIVAB00F'
159900040119     C***                else
160000040119     C***                eval      cmzfla = prmfir
160100040119     C***                eval      cmzmba = prmfir
160200040119     C***                endif
160300040119     C                   eval      cmznrr = *zeros
160400040119     C                   move      §ctrokvb      cmznrr
160500040119     C                   eval      cmzlba = vlrfl1
160600040119     C                   call(e)   'TIS711C'
160700040119     C                   parm                    dscmz
160800040119     C                   parm      *blanks       esito
160900040119     C                   if        %error
161000040119     C                             or cmzerr = '1'
161100040119     C                             or esito  = '1'
161200040119     C                   eval      wrkesito = '3'
161300040119     C                   endif
161400040119     C                   endif
161500040119     C*
161600040119     C                   ENDSR
161700040119     C***
161800010601
161900010601
162000010601
162100010601
162200010601      /TITLE Invio dei dati al punto operativo.
162300010601     C     opeini        BEGSR
162400010601     C*
162500010601     C* Inizializzo flag e contatori operativi
162600010601     C                   movel     '0'           flgGiro           1
162700010601     C                   movel     '0'           flgMulti          1
162800010601     C                   movel     '1'           flgStato          1
162900010615     C                   movel     '0'           flgOk             1
163000010601     C                   z-add     *zeros        cntNonEl         10 0
163100010601     C                   z-add     *zeros        depfil            3 0
163200010601     C                   z-add     *zeros        invfil            3 0
163300010601     C*
163400010601     C                   ENDSR
163500010601     C***
163600070326
163700070326
163800070326
163900070326
164000080916     C     *pssr         BEGSR
164100070329     C*
164200080916     C                   if        %open(tivin00r)
164300080916     C                   close     tivin00r
164400080916     C                   endif
164500080916     C                   if        %open(fivabwwr)
164600080916     C                   close     fivabwwr
164700080916     C                   endif
164800080916     C                   if        %open(fivatwwr)
164900080916     C                   close     fivatwwr
165000080916     C                   endif
165100070326     C*
165200070326     C* Effettuo la chiamata al CLLE preposto
165300080916     C                   call(e)   'TITVVTC'
165400080916     C                   parm                    parccm
165500080916     C                   parm                    parmbr
165600080916     C                   parm      '2'           paropz
165700070326     C*
165800080916     C                   eval      wrkesito = '2'
165900070404     C*
166000080916     C                   seton                                        LR
166100070326     C*
166200080916     C                   ENDSR     '*CANCL'
166300070326     C***
166400070326
166500091117     C*----------------------------------------------------*
166600091117     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
166700091117     C*----------------------------------------------------*
166800091117     C     REPNSP        BEGSR
166900091117     C*
167000091117     C                   EXSR      INZVAR
167100091117     C                   EXSR      DEFCAM
167200091117     C*
167300091117     C* NSP => Stacco un numeratore da AZNUM
167400091117     C                   clear                   TRUL33DS
167500091117     C                   eval      I33OPE = *zeros
167600091117     C                   eval      I33CNU = 302
167700091117     C                   eval      I33NUM = 1
167800091117     C                   movel     TRUL33DS      KPJBU
167900091117     C                   call      'TRUL33R'
168000091117     C                   parm                    KPJBA
168100091117     C                   movel     KPJBU         TRUL33DS
168200091117     C                   if        O33ERR = *zeros
168300091117     C                   z-add     O33NRF        VABNSP
168400091117     C                   z-add     O33NRF        VATNSP
168500091117     C                   else
168600091117     C                   SETON                                        31
168700091117     C                   EVAL      vinmsg = %trimr(vinmsg)
168800091117     C                             + ' ' + 'VABNSP VATNSP'
168900091117     C                   endif
169000091117     C*
169100091117     C                   ENDSR
169200010330
169300010330
169400000613     C     *inzsr        BEGSR
169500990910     C*
169600990910     C     *entry        plist
169700990920     C                   parm                    tivlrds
169800990921     C                   parm      wrkesito      esito
169900000724     C                   parm                    prmlit
170000000710     C                   parm                    prmfir
170100010330     C*
170200010330     C* CALCOLA LA DATA CORRENTE
170300091117     C                   eval      datcor = %dec(%date() : *ISO)
170400000613     C*
170500000613     C                   ENDSR
170600000613     C***
170700990908
170800091117     Otitv1s8p  E            riepilogo         2
170900990915     O                                              'Upload via Internet'
171000990915     O                                           +1 'Traduzione TIVIN00R -'
171100021025     O                                           +1 'FIVABWWR'
171200990915     O                                           +1 'Testate bolle'
171300990915     O          E            riepilogo   2
171400990915     O                       wrkdata
171500990915     O                       wrkora              +1
171600990915     O                       procname            +1
171700990915     O          E            riepilogo   2
171800990915     O                                              'Cliente..................:'
171900010605     O                       VABCCM        z     +1
172000990915     O          E            riepilogo   2
172100990920     O                                              'Riferimento Strategi.....:'
172200990920     O                       vlrhdl              +1
172300990915     O          E            riepilogo   2
172400990915     O                                              'Giusti...................:'
172500040119     O                       §CTROKVB      2   +  1
172600990915     O          E            riepilogo   2
172700990915     O                                              'Sbagliati e corretti.....:'
172800971022     O                       §CTRMO        2   +  1
172900990915     O          E            riepilogo   2
173000990915     O                                              'Sbagliati e scartati.....:'
173100971022     O                       §CTRNO        2   +  1
173200000613
173300091117     Otitv1s8ps E            testdett          2
173400000613     O                                              'Upload via Internet'
173500000613     O                                           +1 'Traduzione TIVIN00R -'
173600021025     O                                           +1 'FIVABWWR'
173700000613     O                                           +1 'Testate bolle'
173800000616     O          E            testdett    3
173900000613     O                                           +2 'N° rec'
174000000613     O                                           +3 'Anteprima contenuto'
174100000616     O          E            rigadett    2
174200000613     O                       rrnum               +2
174300000621     O                       recko               +3
174400000616     O          E            findett     2
174500000613     O                       wrkdata
174600000613     O                       wrkora              +1
174700000613     O                       procname            +1
174800000616     O          E            findett     2
174900000613     O                                              'Cliente..................:'
175000010605     O                       VABCCM        z     +1
175100000616     O          E            findett     2
175200000613     O                                              'Riferimento Strategi.....:'
175300000613     O                       vlrhdl              +1
175400000616     O          E            findett     2
175500000613     O                                              'Giusti...................:'
175600040119     O                       §CTROKVB      2   +  1
175700000616     O          E            findett     2
175800000613     O                                              'Sbagliati e corretti.....:'
175900000613     O                       §CTRMO        2   +  1
176000000616     O          E            findett     2
176100000613     O                                              'Sbagliati e scartati.....:'
176200000613     O                       §CTRNO        2   +  1
176300000613** CMD - COMANDI CL
176400091117OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1S8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
176500091117OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1S8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
176600091117DLTOVR FILE(TITV1S8P TITV1S8PS) LVL(*)
176700010606
176800000613
176900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
177000091117FIL001;VABRMA;FIL002;VABIND;VABCAS;FIL003;VABPKB;VABLOD;W_VABTSP;
177100091117VABNCL;FIL004;FIL005;VABIAS;W_NOTE;W_VABNZD;FIL006;FIL007;VABRSD
177200091117
