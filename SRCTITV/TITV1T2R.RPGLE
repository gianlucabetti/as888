000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800091204     Ftitv1t2p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100091204     Ftitv1t2ps O    f  198        PRINTER usropn
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
002200091204     D intcol          S            100    DIM(7) CTDATA PERRCD(1)
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
006900031201     D*------------------
007000031201     D* Costanti
007100031201     D*------------------
007200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007400080617     D*------------------
007500080617
007600080617     D*------------------
007700080617     D* LINKING A DEFINIZIONI ESTERNE
007800080617     D*------------------
007900080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008000090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008100010330
008200010330
008300010330
008400990915     C                   time                    wrkdata
008500990915     C                   time                    wrkora
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800081014     C                   reset                   wrkesito
008900010601     C*
009000010601     C                   exsr      opeini
009100010605     C                   exsr      rwvab
009200081014     C*
009300081014     C                   exsr      endela
009400010601     C*
009500010601     C                   seton                                        lr
009600010601
009700010601
009800010601
009900010601     C*--------------------------------------------------------
010000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010100010601     C*--------------------------------------------------------
010200010601     C     PREELA        BEGSR
010300010601     C*
010400010601     C* SE OCCORRE SPEDIRE IN FILIALE
010500010601     C                   if        invfil <> *zeros and
010600010601     C                             flgGiro = '0'
010700010601     C*
010800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010900010601     C                   eval      flgGiro = '1'
011000000724     C*
011100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011300010601     C     invfil        chain     azorg01l
011400000724     C                   if        %found
011500000724     C                   movel(p)  CMD(1)        depcmd
011600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011700000724     C*
011800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000724     C                   PARM                    depcmd
012200000724     C                   PARM                    LENGH
012300000724     C*
012400000724     C                   endif
012500000616     C*
012600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012700000616     C                   movel(p)  CMD(2)        depcmd
012800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012900000616     C*
013000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    depcmd
013400000616     C                   PARM                    LENGH
013500000613     C*
013600091204     C                   if        not %open(titv1t2ps)
013700091204     C                   open      titv1t2ps
013800070329     C                   except    testdett
013900000613     C                   endif
014000010601     C*
014100010601     C                   endif
014200010601     C*
014300010601     C                   ENDSR
014400010601     C***
014500010601
014600010601
014700010601
014800010601
014900010601     C*--------------------------------------------------------
015000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015100010601     C*--------------------------------------------------------
015200010601     C     ENDELA        BEGSR
015300081014     C*
015400081014     C                   if        %open(tivin00r)
015500081014     C                   close     tivin00r
015600081014     C                   endif
015700081014     C                   if        %open(fivabwwr)
015800081014     C                   close     fivabwwr
015900081014     C                   endif
016000081014     C                   if        %open(fivatwwr)
016100081014     C                   close     fivatwwr
016200081014     C                   endif
016300040119     C*
016400040119     C* Effettuo la chiamata al CLLE preposto
016500040119     C                   call(e)   'TITVVTC'
016600040510     C                   parm                    parccm
016700040119     C                   parm                    parmbr
016800040119     C                   parm      '2'           paropz
016900050201     C*
017000050201     C* Effettuo lancio TISI95 solo x chiusura
017100050201     C                   CLEAR                   TISI95DS
017200050201     C                   EVAL      I95TLA = 'C'
017300050201     C                   CALL      'TISI95R'
017400050201     C                   PARM                    TISI95DS
017500000613     C*
017600091204     C                   if        %open(titv1t2ps)
017700000616     C                   except    findett
017800091204     C                   close     titv1t2ps
017900000613     C                   endif
018000000616     C*
018100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018200011113     C                   Z-ADD     150           LENGH            15 5
018300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018400000616     C                   PARM                    CMD(3)
018500000616     C                   PARM                    LENGH
018600000616     C*
018700010601     C                   ENDSR
018800010601     C***
018900010601
019000010601
019100010601
019200000613
019300000613     C*--------------------------------------------------------
019400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019500000613     C*--------------------------------------------------------
019600000621     C     STPR          BEGSR
019700000613     C*
019800091204     C                   if        not %open(titv1t2p)
019900091204     C   50              open      titv1t2p
020000990915     C                   endif
020100990915     C*
020200030715     C   50              except    riepilogo
020300990915     C*
020400091204     C                   if        %open(titv1t2p)
020500091204     C   50              close     titv1t2p
020600990914     C                   endif
020700000613     C*
020800000613     C                   ENDSR
020900000613     C***
021000000613
021100010601
021200010601
021300010330     C*--------------------------------------------------------
021400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021500010330     C*--------------------------------------------------------
021600010605     C     RWVAB         BEGSR
021700010330     C*
021800010330     C                   if        not %open(tivin00r)
021900010330     C                   open      tivin00r
022000010330     C                   endif
022100021025     C                   if        not %open(fivabwwr)
022200021025     C                   open      fivabwwr
022300010330     C                   endif
022400040119     C*
022500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022600040119     C                   exsr      prevat
022700040119     C*
022800040119     C                   if        chkcall = '0'
022900040119     C*
023000040119     C                   if        not %open(fivatwwr)
023100040119     C                   open      fivatwwr
023200040119     C                   endif
023300010330     C*
023400040119     C                   clear                   §CTROKVB          7 0
023500040119     C                   clear                   §CTROKVT          7 0
023600040119     C                   clear                   §CTRMO            7 0
023700040119     C                   clear                   §CTRNO            7 0
023800070823     C*
023900070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024000070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024100090420     C                   exsr      inzvar
024200090420     C                   exsr      impvab
024300010330     C*
024400030822     C                   do        *HIVAL
024500010330     C*
024600030822     C                   read      tivin00r                               70
024700010618     C*
024800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024900010618     C                   if        vinflg = '1'
025000010618     C                   eval      flgOk = '1'
025100010618     C                   endif
025200040510     C*
025300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025400040510     C                   z-add     *zeros        wLenVINDTA        4 0
025500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
025600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
025700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
025800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
025900040510     C                   z-add     1             wFlgVINDTA
026000040510     C                   leave
026100040510     C                   endif
026200040510     C                   enddo
026300090420     C*
026400090420     C                   add       1             rrnum
026500010618     C*
026600040510     C                   if        vindta > *blanks AND
026700090420     C                             wFlgVINDTA = 1
026800010330     C*
026900010601     C                   if        *in70 = *off and
027000010330     C                             (vinflg = *blanks
027100010330     C                              or vinflg = '0'
027200010330     C                              or vinflg = '2')
027300010330     C*
027400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027500010711     C                   if        vinflg = *blanks or vinflg = '0'
027600010711     C                   clear                   vinmsg
027700010711     C                   endif
027800010601     C*
027900090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
028000090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
028100090421     C                   eval      vindta  = '*' + %trim(vindta)
028200090421     C                   endif
028300091204     C*
028400091204     C* Se presente il delimitatore testo lo sfrutto  x testare che
028500091204     C* nn sia presente il carattere separatore campo tra 2 delimitatori testo
028600091204     C                   if        CharTXT <> *blanks
028700091204     C                   z-add     *zeros        wLenVINDTA        4 0
028800091204     C                   setoff                                       65
028900091204     C                   dow       wLenVINDTA < %len(vindta)
029000091204     C                   eval      wLenVINDTA = wLenVINDTA + 1
029100091204     C                   if        %subst(vindta:wLenVINDTA:1) = CharTXT
029200091204     C                   if        *in65
029300091204     C                   setoff                                       65
029400091204     C                   else
029500091204     C                   seton                                        65
029600091204     C                   endif
029700091204     C                   endif
029800091204     C                   if        %subst(vindta:wLenVINDTA:1) = CharCSV
029900091204     C                             AND *in65
030000091204     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
030100091204     C                   endif
030200091204     C                   enddo
030300091204     C                   endif
030400070925     C*
030500010330     C                   exsr      inzvar
030600010605     C                   exsr      impvab
030700010601     C*
030800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030900010605     C                   eval      depfil = VABLNP
031000010601     C                   exsr      repfil
031100010601     C                   if        depfil = invfil
031200021025     C                   if        vlrpoi = 999
031300030822     C                   move(P)   invfil        VABFGS
031400021025     C                   else
031500030822     C                   move(P)   vlrpoi        VABFGS
031600021025     C                   endif
031700010601     C*
031800010601     C                   exsr      PREELA
031900010601     C*
032000010604     C* Ebbene...
032100010604     C*
032200040119     C  N31              add       1             §CTROKVB
032300040119     C   32              add       1             §CTRMO
032400040119     C   31              add       1             §CTRNO
032500030822     C                   if        wGiro = 2
032600040510     C* Scrivo sempre il VAT
032700040510     C  N31              exsr      wrivat
032800040510     C* Scrivo il VAB solo a rottura d spedizione
032900040119     C                   if        depspe =  *zeros OR
033000040119     C                             depspe <> vabNSP
033100040119     C  N31              exsr      wrivab
033200040119     C  N31              eval      depspe = vabNSP
033300040119     C                   endif
033400040119     C                   endif
033500010604     C*
033600010604     C                   if        *in31 = *off and
033700010604     C                             *in32 = *off
033800010604     C                   eval      vinflg = '1'
033900010604     C                   else
034000010604     C                   eval      recko = vindta
034100010604     C                   except    rigadett
034200010604     C                   eval      vinflg = '2'
034300010604     C                   endif
034400010604     C                   endif
034500010601     C*
034600010604     C                   endif
034700010604     C*
034800010330     C                   else
034900010330     C                   eval      vinflg = '1'
035000010330     C                   endif
035100010601     C*
035200010601     C  N70              update    tivin000
035300010330     C*
035400030822     C  N70              enddo
035500040119     C*
035600040119     C                   endif                                                  (endif - chkcall)
035700010601     C*
035800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035900010601     C                   if        cntNonEl = *zeros or
036000010601     C                             flgMulti = '0'
036100010330     C* Se non ci sono record con errori ...
036200010601     C                   if        §ctrno = 0 and
036300010604     C                             §ctrmo = 0 and
036400010601     C                             flgStato <> '2'
036500010330     C* ... restituisco esito OK.
036600010330     C                   eval      wrkesito = '0'
036700010330     C                   else
036800040119     C                   if        §ctrokvb > 0
036900010330     C                   eval      wrkesito = '1'
037000010330     C                   else
037100010615     C                   if        flgOk = '0'
037200010615     C                   eval      wrkesito = '2'
037300010615     C                   else
037400010615     C                   eval      wrkesito = '6'
037500010615     C                   endif
037600010330     C                   endif
037700010330     C                   endif
037800010601     C                   else
037900010601     C                   eval      wrkesito = '9'
038000010601     C                   endif
038100010330     C*
038200010330     C                   if        %open(tivin00r)
038300010330     C                   close     tivin00r
038400010330     C                   endif
038500021025     C                   if        %open(fivabwwr)
038600021025     C                   close     fivabwwr
038700010330     C                   endif
038800040512     C                   if        %open(fivatwwr)
038900040512     C                   close     fivatwwr
039000040512     C                   endif
039100010601     C*
039200010601     C                   if        vlrpoi <> 999
039300010601     C                   eval      invfil = vlrpoi
039400010601     C                   endif
039500010330     C*
039600040119     C                   if        §ctrokvb > 0
039700010601     C                             and invfil > *zeros
039800010330     C                   exsr      invio
039900010330     C                   endif
040000010601     C*
040100010618     C                   if        flgGiro = '1'
040200081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
040300010618     C                   endif
040400010330     C*
040500010330     C                   ENDSR
040600010330     C***
040700040119
040800040119
040900040119
041000040119     C*----------------------------------------------------*
041100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
041200040119     C*----------------------------------------------------*
041300040510     C     WRIVAB        BEGSR
041400040119     C*
041500060331     C* Gestisco l'eventuale rottura x numero spedizione
041600070823     C                   if        VABNSP <> depspe
041700070301     C                   if        wVATNOT_E <> *blanks
041800080125     C                   if        VABCTM = *blanks
041900080125     C                   eval      VABCTM = '7Q'
042000080125     C                   endif
042100070301     C                   endif
042200040119     C                   write     fivab000                                     => scarico il VAB
042300060331     C                   endif
042400040119     C*
042500040119     C                   ENDSR
042600040119
042700040119
042800040119
042900040119     C*----------------------------------------------------*
043000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
043100040119     C*----------------------------------------------------*
043200040510     C     WRIVAT        BEGSR
043300060512     C*
043400060512     C* Inizializzo i campi chiave
043500060512     C                   eval      VATFGS = VABFGS
043600060512     C                   eval      VATAAS = VABAAS
043700060512     C                   eval      VATCCM = VABCCM
043800070823     C                   eval      VATNRS = VABNRS
043900060512     C                   eval      VATNSP = VABNSP
044000060512     C                   eval      VATLNP = VABLNP
044100060331     C*
044200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
044300060331     C                   if        wVATNOT_A <> *blanks
044400060331     C                   eval      VATNOT = wVATNOT_A
044500060331     C                   eval      VATTRC = 'A'
044600060331     C                   write     FIVAT000
044700060331     C                   add       1             §CTROKVT
044800060331     C                   endif
044900060331     C*
045000060331     C                   if        wVATNOT_B <> *blanks
045100060331     C                   eval      VATNOT = wVATNOT_B
045200060331     C                   eval      VATTRC = 'B'
045300060331     C                   write     FIVAT000
045400060331     C                   add       1             §CTROKVT
045500060331     C                   endif
045600060331     C*
045700080625     C                   if        wVATNOT_E <> *blanks
045800060331     C                   eval      VATNOT = wVATNOT_E
045900060331     C                   eval      VATTRC = 'E'
046000080716     C                   write     FIVAT000
046100080716     C                   add       1             §CTROKVT
046200080625     C                   endif
046300040119     C*
046400040119     C                   ENDSR
046500040119
046600040119
046700040119
046800040119     C*----------------------------------------------------*
046900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047000040119     C*----------------------------------------------------*
047100040119     C     PREVAT        BEGSR
047200040119     C*
047300040119     C* Compongo il nome del membro da dare al FIVATWWR
047400040119     C                   eval      parmbr = vlrhdl
047500040119     C                   movel     'M'           parmbr
047600040510     C                   eval      parccm = vlrksc
047700040119     C                   eval      paropz = '1'
047800040119     C* Effettuo la chiamata al CLLE preposto
047900040119     C                   call(e)   'TITVVTC'
048000040119     C                   parm                    parccm
048100040119     C                   parm                    parmbr
048200040119     C                   parm                    paropz
048300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048400040119     C                   if        %error
048500040119     C                   movel     '1'           chkcall
048600040119     C                   else
048700040119     C                   movel     '0'           chkcall
048800040119     C                   endif
048900040119     C*
049000040119     C                   ENDSR
049100010601
049200010601
049300010601
049400010330     C*----------------------------------------------------*
049500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049600010330     C*----------------------------------------------------*
049700040119     C     INZVAR        BEGSR
049800010330     C*
049900030822     C* Inizializzo variabili di wrk
050000020204     C                   Z-ADD     *zeros        Num5_0            5 0
050100060331     C                   MOVEL     *blanks       wVATNOT_A
050200060331     C                   MOVEL     *blanks       wVATNOT_B
050300060331     C                   MOVEL     *blanks       wVATNOT_E
050400090220     C*
050500080617     C                   EVAL      wFlgCAS = *blanks
050600030822     C*
050700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050800030822     C                   CLEAR                   FIVAB000
050900040119     C                   CLEAR                   FIVAT000
051000030822     C                   CLEAR                   SkSplitCSV
051100030822     C                   EVAL      i = 1
051200030822     C                   EVAL      posDa = *zeros
051300030822     C                   EVAL      posA  = *zeros
051400030822     C*
051500030822     C* Reimposto i valori di default
051600030822     C                   EXSR      DEFCAM
051700030822     C*
051800010330     C                   ENDSR
051900010330     C*----------------------------------------------------*
052000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052100010330     C*----------------------------------------------------*
052200010330     C     DEFCAM        BEGSR
052300030822     C*
052400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052500030715     C* e delimitatore testo.
052600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052900030822     C*
053000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
053200030715     C                   SETON                                        50
053300030715     C                   ELSE
053400030715     C                   SETOFF                                       50
053500030715     C                   ENDIF
053600090421     C*
053700090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
053800090421     C                   EVAL      VABCBO = '1'
053900090421     C                   EVAL      VABLNP = vlrpoi
054000090421     C                   EVAL      VABNSP = rrnum
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
082900090716     C                             %trim(intcol(3))+%trim(intcol(4))+
083000091204     C                             %trim(intcol(5))+%trim(intcol(6))+
083100091204     C                             %trim(intcol(7))<>*blanks
083200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083300090716     C                                      %trim(intcol(3))+%trim(intcol(4))+
083400091204     C                                      %trim(intcol(5))+%trim(intcol(6))+
083500091204     C                                      %trim(intcol(7))
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
117800090716     C                             %trim(SkSplitCSV(i)) <> '.0'     AND
117900090716     C                             %trim(SkSplitCSV(i)) <> '.00'    AND
118000090716     C                             %trim(SkSplitCSV(i)) <> ',0'     AND
118100090716     C                             %trim(SkSplitCSV(i)) <> ',00'    AND
118200090716     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
118300090716     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
118400090716     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
118500081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
118600080125     C                   EVAL      wFlgCAS = '1'
118700080125     C                   ENDIF
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100030721     C                   Z-ADD     PiVal         VABCAS
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABCAS = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABCAS'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** CCM
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on      AND
120400030721     C                             PiVal<=9999999 AND
120500030721     C                             PiVal>*zeros
120600030721     C                   Z-ADD     PiVal         VABCCM
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      VABCCM = *zeros
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100040510     C                             + ' ' + 'VABCCM'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** LNP
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on    AND
121900030721     C                             PiVal<=999   AND
122000030721     C                             PiVal>*zeros
122100030721     C                   Z-ADD     PiVal         VABLNP
122200030721     C                   ELSE
122300040715     C                   SETON                                        32
122400030721     C                   EVAL      VABLNP = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600040510     C                             + ' ' + 'VABLNP'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** NRS
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on AND
123400030721     C                             PiVal<=99
123500030721     C                   Z-ADD     PiVal         VABNRS
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABNRS'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** LNA
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on    AND
124700030721     C                             PiVal<=999   AND
124800030721     C                             PiVal>*zeros
124900030721     C                   Z-ADD     PiVal         VABLNA
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      VABLNA = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABLNA'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** CTR
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on  AND
126200030721     C                             PiVal<=999
126300030721     C                   Z-ADD     PiVal         VABCTR
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABCTR'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** IAS
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiNum=*on
127500030721     C                   Z-ADD     PiVal         VABIAS
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABIAS'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** VLB
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiNum=*on
128700030721     C                   Z-ADD     PiVal         VABVLB
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABVLB'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** QFT
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiNum=*on
129900030721     C                   Z-ADD     PiVal         VABQFT
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABQFT'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** NCD
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on      AND
131100030721     C                             PiVal<=9999999
131200030721     C                   Z-ADD     PiVal         VABNCD
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABNCD'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** NCA
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on      AND
132400030721     C                             PiVal<=9999999
132500030721     C                   Z-ADD     PiVal         VABNCA
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABNCA'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** ZNC
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on AND
133700030721     C                             PiVal<=99
133800030721     C                   Z-ADD     PiVal         VABZNC
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABZNC'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** DCR
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on       AND
135000030721     C                             PiVal<=99999999
135100030721     C                   Z-ADD     PiVal         VABDCR
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABDCR'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** HCR
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on   AND
136300030721     C                             PiVal<=9999
136400030721     C                   Z-ADD     PiVal         VABHCR
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABHCR'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** VMD
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiNum=*on
137600030721     C                   Z-ADD     PiVal         VABVMD
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABVMD'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** ANT
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on        AND
138800030721     C                             PiVal<=999999999
138900030721     C                   Z-ADD     PiVal         VABANT
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABANT'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** CMO
139700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiInt=*on
140100030721     C                   Z-ADD     PiVal         Num5_0
140200030721     C                   MOVEL(p)  Num5_0        VABCMO
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      VABCMO = *zeros
140600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140700030721     C                             + ' ' + 'VABCMO'
140800030721     C                   ENDIF
140900030721     C                   ENDIF
141000040510     C*
141100040510     C* Reperisco i campi del FIVAT
141200040510     C*** VATNOT
141300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141500040510     C                   ENDIF
141600040510     C*** VATTRC
141700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141900040510     C                   ENDIF
142000060331     C*** VATNOT_A
142100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142300060331     C                   ENDIF
142400060331     C*** VATNOT_B
142500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142700060331     C                   ENDIF
142800060331     C*** VATNOT_E
142900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143100060331     C                   ENDIF
143200070823     C*
143300071029     C***  ===>  Gestione campi "particolari" dentro ciclo
143400090421     C*
143500070925     C*
143600071029     C***  <===  -----------------------------------------
143700040510     C*
143800030715     C                   ENDIF
143900030715     C                   EVAL      i = i + 1
144000030715     C                   ENDDO
144100070925     C*
144200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144300080125     C*
144400070925     C*
144500071029     C***  <===  -----------------------------------------
144600070925     C*
144700070925     C* Considerazioni finali su CBO/CAS
144800080125     C                   IF        wFlgCAS = '1'
144900070925     C                   IF        VABCBO = '1'
145000070925     C                   EVAL      VABCBO = '4'
145100070925     C                   ENDIF
145200070925     C                   IF        VABCBO = '2'
145300070925     C                   EVAL      VABCBO = '6'
145400070925     C                   ENDIF
145500070925     C                   ENDIF
145600070925     C***  <===  ----------------------------
145700040510     C*
145800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145900040510     C                   IF        VABNSP = *zeros
146000040510     C                   SETON                                        31
146100040510     C                   ENDIF
146200040715     C*
146300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146400040715     C                   IF        VABLNP = *zeros
146500040715     C                   SETON                                        31
146600040715     C                   ENDIF
146700040510     C*
146800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146900040510     C                   IF        VABRMN = *zeros
147000040510     C                   Z-ADD     VABNSP        VABRMN
147100040510     C                   ENDIF
147200040510     C*
147300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147400040510     C                   IF        VABRMA = *blanks
147500040714     C                   MOVEL     VABRMN        VABRMA
147600040510     C                   ENDIF
147700040128     C*
147800040510     C* Se provincia nn valorizzata la reperisco
147900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148000040510     C                   IF        VABPRD = *blanks
148100040128     C                   CLEAR                   TISI95DS
148200040128     C                   EVAL      I95TCN = '3'
148300040128     C                   Z-ADD     datcor        I95DAT
148400040128     C                   EVAL      I95NAR = VABNZD
148500040128     C                   EVAL      I95CAP = VABCAD
148600040128     C                   EVAL      I95LOC = VABLOD
148700040128     C                   CALL      'TISI95R'
148800040128     C                   PARM                    TISI95DS
148900040128     C                   EVAL      VABPRD = O95PRV
149000040510     C                   ENDIF
149100020204     C*
149200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149300020204     C                   EXSR      CHKIMPDIV
149400030715     C                   ENDIF
149500020204     C*
149600010607     C                   ENDSR
149700010607     C*----------------------------------------------------*
149800010601
149900020204
150000020204     C*----------------------------------------------------*
150100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150200020204     C*----------------------------------------------------*
150300020204     C     CHKIMPDIV     BEGSR
150400020204     C*
150500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150600020204     C                   Z-ADD     *zeros        wrkDec            9 9
150700020204     C*
150800020204     C* Come prima cosa effettuo considerazioni sulla divisa
150900020204     C                   IF        vabIAS > *zeros
151000020204     C                   IF        vabVAS <> 'EUR'
151100020204     C                   EVAL      vabVAS =  'ITL'
151200020204     C                   ENDIF
151300020204     C                   ENDIF
151400020204     C*
151500020204     C                   IF        vabCAS > *zeros
151600020204     C                   IF        vabVCA <> 'EUR'
151700020204     C                   EVAL      vabVCA =  'ITL'
151800020204     C                   ENDIF
151900020204     C                   ENDIF
152000020204     C*
152100020204     C                   IF        vabVMD > *zeros
152200020204     C                   IF        vabVAD <> 'EUR'
152300020204     C                   EVAL      vabVAD =  'ITL'
152400020204     C                   ENDIF
152500020204     C                   ENDIF
152600020204     C*
152700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152800020204     C                   Z-ADD     vabIAS        wrkDec
152900020204     C                   IF        wrkDec > *zeros
153000020204     C                   IF        vabVAS = 'ITL'
153100020204     C                   EVAL      vabIAS = *zeros
153200020204     C                   ENDIF
153300020204     C                   ENDIF
153400020204     C*
153500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153600020204     C                   Z-ADD     vabCAS        wrkDec
153700020204     C                   IF        wrkDec > *zeros
153800020204     C                   IF        vabVCA = 'ITL'
153900020204     C                   EVAL      vabCAS = *zeros
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154400020204     C                   Z-ADD     vabVMD        wrkDec
154500020204     C                   IF        wrkDec > *zeros
154600020204     C                   IF        vabVAD = 'ITL'
154700020204     C                   EVAL      vabVMD = *zeros
154800020204     C                   ENDIF
154900020204     C                   ENDIF
155000020204     C*
155100020204     C                   ENDSR
155200020204     C***
155300080617
155400080617
155500080617
155600080617     C*----------------------------------------------------*
155700080617     C*  CONTROLLO NUMERICITA' CAMPI
155800080617     C*----------------------------------------------------*
155900080617     C     CHKNUM        BEGSR
156000080617     C*
156100080617     C                   IF        PiDecChr = *blanks
156200080617     C                   EVAL      PiDecChr = CharNUM
156300080617     C                   ENDIF
156400080617     C*
156500080617     C                   callp     UBISNUM_Check(PiStr
156600080617     C                                          :PiDecChr
156700080617     C                                          :PiVal
156800080617     C                                          :PiNum
156900080617     C                                          :PiInt)
157000080617     C*
157100080617     C                   ENDSR
157200080617     C***
157300010330
157400010601
157500010601
157600010601
157700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157800010601     C     repfil        BEGSR
157900010601     C*
158000010601     C                   if        invfil = *zeros and
158100010601     C                             depfil > *zeros and
158200010629     C                             (vinflg = *blanks or
158300010629     C                              vinflg = *zeros)
158400010601     C
158500010601     C                   eval      invfil = depfil
158600010601     C                   endif
158700010601     C*
158800010601     C                   if        depfil <> invfil and
158900010601     C                             invfil > *zeros
159000010601     C                   eval      flgMulti = '1'
159100010601     C                   if        vinflg = *blanks
159200010601     C                   add       1             cntNonEl
159300010601     C                   endif
159400010601     C                   endif
159500010601     C*
159600010601     C                   if        vinflg = '2'
159700010601     C                   eval      flgStato = '2'
159800010601     C                   endif
159900010601     C*
160000010601     C                   ENDSR
160100010601     C***
160200010601
160300010601
160400010601
160500010330
160600040119      /TITLE Invio dei dati al punto operativo.
160700040119     C     invio         BEGSR
160800040119     C*
160900040119     C* 1° invio FIVAT
161000040119     C                   reset                   dscmz
161100040119     C                   move      vlrpoi        cmzdst
161200040119     C                   eval      cmzfld = 'FIVATWWR'
161300040119     C                   eval      cmzmbd = vlrhdl
161400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161500040119     C***                if        prmfir = *blanks
161600040119     C                   eval      cmzfla = 'FIVAT00F'
161700040119     C                   eval      cmzmba = 'FIVAT00F'
161800040119     C***                else
161900040119     C***                eval      cmzfla = prmfir
162000040119     C***                eval      cmzmba = prmfir
162100040119     C***                endif
162200040119     C                   eval      cmznrr = *zeros
162300040119     C                   move      §ctrokvt      cmznrr
162400040119     C                   eval      cmzlba = vlrfl1
162500040119     C                   call(e)   'TIS711C'
162600040119     C                   parm                    dscmz
162700040119     C                   parm      *blanks       esito
162800040119     C                   if        %error
162900040119     C                             or cmzerr = '1'
163000040119     C                             or esito  = '1'
163100040119     C                   eval      wrkesito = '3'
163200040119     C                   else
163300040119     C*
163400040119     C* 2° invio FIVAB
163500040119     C                   reset                   dscmz
163600040119     C                   move      vlrpoi        cmzdst
163700040119     C                   eval      cmzfld = vlrfou
163800040119     C                   eval      cmzmbd = vlrhdl
163900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164000040119     C***                if        prmfir = *blanks
164100040119     C                   eval      cmzfla = 'FIVAB00F'
164200040119     C                   eval      cmzmba = 'FIVAB00F'
164300040119     C***                else
164400040119     C***                eval      cmzfla = prmfir
164500040119     C***                eval      cmzmba = prmfir
164600040119     C***                endif
164700040119     C                   eval      cmznrr = *zeros
164800040119     C                   move      §ctrokvb      cmznrr
164900040119     C                   eval      cmzlba = vlrfl1
165000040119     C                   call(e)   'TIS711C'
165100040119     C                   parm                    dscmz
165200040119     C                   parm      *blanks       esito
165300040119     C                   if        %error
165400040119     C                             or cmzerr = '1'
165500040119     C                             or esito  = '1'
165600040119     C                   eval      wrkesito = '3'
165700040119     C                   endif
165800040119     C                   endif
165900040119     C*
166000040119     C                   ENDSR
166100040119     C***
166200010601
166300010601
166400010601
166500010601
166600010601      /TITLE Invio dei dati al punto operativo.
166700010601     C     opeini        BEGSR
166800010601     C*
166900010601     C* Inizializzo flag e contatori operativi
167000010601     C                   movel     '0'           flgGiro           1
167100010601     C                   movel     '0'           flgMulti          1
167200010601     C                   movel     '1'           flgStato          1
167300010615     C                   movel     '0'           flgOk             1
167400010601     C                   z-add     *zeros        cntNonEl         10 0
167500010601     C                   z-add     *zeros        depfil            3 0
167600010601     C                   z-add     *zeros        invfil            3 0
167700010601     C*
167800010601     C                   ENDSR
167900010601     C***
168000070326
168100070326
168200070326
168300070326
168400080916     C     *pssr         BEGSR
168500070329     C*
168600080916     C                   if        %open(tivin00r)
168700080916     C                   close     tivin00r
168800080916     C                   endif
168900080916     C                   if        %open(fivabwwr)
169000080916     C                   close     fivabwwr
169100080916     C                   endif
169200080916     C                   if        %open(fivatwwr)
169300080916     C                   close     fivatwwr
169400080916     C                   endif
169500070326     C*
169600070326     C* Effettuo la chiamata al CLLE preposto
169700080916     C                   call(e)   'TITVVTC'
169800080916     C                   parm                    parccm
169900080916     C                   parm                    parmbr
170000080916     C                   parm      '2'           paropz
170100070326     C*
170200080916     C                   eval      wrkesito = '2'
170300070404     C*
170400080916     C                   seton                                        LR
170500070326     C*
170600080916     C                   ENDSR     '*CANCL'
170700070326     C***
170800070326
170900070326
171000010330
171100010330
171200000613     C     *inzsr        BEGSR
171300990910     C*
171400990910     C     *entry        plist
171500990920     C                   parm                    tivlrds
171600990921     C                   parm      wrkesito      esito
171700000724     C                   parm                    prmlit
171800000710     C                   parm                    prmfir
171900010330     C*
172000010330     C* CALCOLA LA DATA CORRENTE
172100091204     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
172200091204     C                   eval      datcor = %dec(%date() : *ISO)
172300000613     C*
172400000613     C                   ENDSR
172500000613     C***
172600990908
172700091204     Otitv1t2p  E            riepilogo         2
172800990915     O                                              'Upload via Internet'
172900990915     O                                           +1 'Traduzione TIVIN00R -'
173000021025     O                                           +1 'FIVABWWR'
173100990915     O                                           +1 'Testate bolle'
173200990915     O          E            riepilogo   2
173300990915     O                       wrkdata
173400990915     O                       wrkora              +1
173500990915     O                       procname            +1
173600990915     O          E            riepilogo   2
173700990915     O                                              'Cliente..................:'
173800010605     O                       VABCCM        z     +1
173900990915     O          E            riepilogo   2
174000990920     O                                              'Riferimento Strategi.....:'
174100990920     O                       vlrhdl              +1
174200990915     O          E            riepilogo   2
174300990915     O                                              'Giusti...................:'
174400040119     O                       §CTROKVB      2   +  1
174500990915     O          E            riepilogo   2
174600990915     O                                              'Sbagliati e corretti.....:'
174700971022     O                       §CTRMO        2   +  1
174800990915     O          E            riepilogo   2
174900990915     O                                              'Sbagliati e scartati.....:'
175000971022     O                       §CTRNO        2   +  1
175100000613
175200091204     Otitv1t2ps E            testdett          2
175300000613     O                                              'Upload via Internet'
175400000613     O                                           +1 'Traduzione TIVIN00R -'
175500021025     O                                           +1 'FIVABWWR'
175600000613     O                                           +1 'Testate bolle'
175700000616     O          E            testdett    3
175800000613     O                                           +2 'N° rec'
175900000613     O                                           +3 'Anteprima contenuto'
176000000616     O          E            rigadett    2
176100000613     O                       rrnum               +2
176200000621     O                       recko               +3
176300000616     O          E            findett     2
176400000613     O                       wrkdata
176500000613     O                       wrkora              +1
176600000613     O                       procname            +1
176700000616     O          E            findett     2
176800000613     O                                              'Cliente..................:'
176900010605     O                       VABCCM        z     +1
177000000616     O          E            findett     2
177100000613     O                                              'Riferimento Strategi.....:'
177200000613     O                       vlrhdl              +1
177300000616     O          E            findett     2
177400000613     O                                              'Giusti...................:'
177500040119     O                       §CTROKVB      2   +  1
177600000616     O          E            findett     2
177700000613     O                                              'Sbagliati e corretti.....:'
177800000613     O                       §CTRMO        2   +  1
177900000616     O          E            findett     2
178000000613     O                                              'Sbagliati e scartati.....:'
178100000613     O                       §CTRNO        2   +  1
178200000613** CMD - COMANDI CL
178300091204OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1T2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
178400091204OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1T2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
178500091204DLTOVR FILE(TITV1T2P TITV1T2PS) LVL(*)
178600010606
178700000613
178800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178900091204VABATB,VABCCM,VABLNP,VABAAS,VABMGS,VABNRS,VABNSP,VABCBO,VABLNA,VABRSD,VABRD2,
179000091204VABIND,VABCAD,VABLOD,VABPRD,VABNZD,VABGC1,VABGC2,VABCTR,VABTSP,VABIAS,VABVAS,
179100091204VABNAS,VABNCL,VABPKB,VABVLB,VABQFT,VABCAS,VABTIC,VABVCA,VABGCA,VABRMN,VABRMA,
179200091204VABNCD,VABNCA,VABXCO,VABNOT,VABNT2,VABZNC,VABCTM,VABFFD,VABDCR,VABTCR,VABHCR,
179300091204VABCTS,VABFTM,VABVMD,VABVAD,VABGMA,VABGGA,VABGVA,VABTC1,VABTC2,VABSCL,VABANT,
179400091204VABRMO,VABCMO,VABNMO
179500091204
