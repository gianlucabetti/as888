000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800090603     Ftitv1q0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100090603     Ftitv1q0ps O    f  198        PRINTER usropn
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
004000040510     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400080125     D wFlgCAS         s              1    INZ(*blanks)
004500080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004600030715     D*------------------
004700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004800030715     D*------------------
004900030715     D SkSplitFLD      S             10    DIM(100)
005000030715     D SkSplitCSV      S            256    DIM(100)
005100030715     D CharCSV         S              1
005200030715     D CharTXT         S              1
005300030715     D CharNUM         S              1
005400030715     D posDa           S              3  0 INZ(*zeros)
005500030715     D posA            S              3  0 INZ(*zeros)
005600030715     D i               s              3  0 INZ(1)
005700030715     D wGiro           s              1  0 INZ(*zeros)
005800080125     D wVATNOT_A       s                   LIKE(VATNOT)
005900080125     D wVATNOT_B       s                   LIKE(VATNOT)
006000080125     D wVATNOT_E       s                   LIKE(VATNOT)
006100030822     D*------------------
006200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006300030822     D*------------------
006400030822     D posDaDft        S              3  0 INZ(*zeros)
006500030822     D posADft         S              3  0 INZ(*zeros)
006600030822     D j               s              3  0 INZ(1)
006700030822     D wGiroDft        s              1  0 INZ(*zeros)
006800010330     D*------------------
006900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007000010330     D*------------------
007100010330     D WLBDA8          DS                  INZ
007200010330     D  G08DAT                 1      8  0
007300010330     D  G08INV                 9     16  0
007400010330     D  G08ERR                17     17
007500010330     D  G08TGI                18     22  0
007600090709     D*------------------
007700090709     D* DS REPERIMENTO NUMERATORE
007800090709     D*------------------
007900090709     D trul33ds      e ds                  inz
008000090709     D*------------------
008100090709     D* DS ARCHITETTURA
008200090709     D*------------------
008300090709     D kpjba         e ds                  inz
008400090709
008500031201     D*------------------
008600031201     D* Costanti
008700031201     D*------------------
008800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009000080617     D*------------------
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
015200090603     C                   if        not %open(titv1q0ps)
015300090603     C                   open      titv1q0ps
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
019200090603     C                   if        %open(titv1q0ps)
019300000616     C                   except    findett
019400090603     C                   close     titv1q0ps
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
021400090603     C                   if        not %open(titv1q0p)
021500090603     C   50              open      titv1q0p
021600990915     C                   endif
021700990915     C*
021800030715     C   50              except    riepilogo
021900990915     C*
022000090603     C                   if        %open(titv1q0p)
022100090603     C   50              close     titv1q0p
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
028000090420     C*****
028100090420     C* Escludo la prima riga
028200090420     C                   if        %subst(vindta:1:2) = 'R|'
028300090420     C                   z-add     *zeros        wFlgVINDTA
028400090420     C                   endif
028500090420     C*****
028600090420     C*
028700090420     C                   add       1             rrnum
028800010618     C*
028900040510     C                   if        vindta > *blanks AND
029000090420     C                             wFlgVINDTA = 1
029100010330     C*
029200010601     C                   if        *in70 = *off and
029300010330     C                             (vinflg = *blanks
029400010330     C                              or vinflg = '0'
029500010330     C                              or vinflg = '2')
029600010330     C*
029700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029800010711     C                   if        vinflg = *blanks or vinflg = '0'
029900010711     C                   clear                   vinmsg
030000010711     C                   endif
030100010601     C*
030200090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
030300090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
030400090421     C                   eval      vindta  = '*' + %trim(vindta)
030500090421     C                   endif
030600070925     C*
030700010330     C                   exsr      inzvar
030800010605     C                   exsr      impvab
030900010601     C*
031000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
031100010605     C                   eval      depfil = VABLNP
031200010601     C                   exsr      repfil
031300010601     C                   if        depfil = invfil
031400021025     C                   if        vlrpoi = 999
031500030822     C                   move(P)   invfil        VABFGS
031600021025     C                   else
031700030822     C                   move(P)   vlrpoi        VABFGS
031800021025     C                   endif
031900010601     C*
032000010601     C                   exsr      PREELA
032100010601     C*
032200010604     C* Ebbene...
032300010604     C*
032400040119     C  N31              add       1             §CTROKVB
032500040119     C   32              add       1             §CTRMO
032600040119     C   31              add       1             §CTRNO
032700030822     C                   if        wGiro = 2
032800040510     C* Scrivo sempre il VAT
032900040510     C  N31              exsr      wrivat
033000090709     C* Essendo tutte spedizioni mono-collo scrivo sempre anche il VAB
033100040119     C  N31              exsr      wrivab
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
035000010601     C  N70              update    tivin000
035100010330     C*
035200030822     C  N70              enddo
035300040119     C*
035400040119     C                   endif                                                  (endif - chkcall)
035500010601     C*
035600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035700010601     C                   if        cntNonEl = *zeros or
035800010601     C                             flgMulti = '0'
035900010330     C* Se non ci sono record con errori ...
036000010601     C                   if        §ctrno = 0 and
036100010604     C                             §ctrmo = 0 and
036200010601     C                             flgStato <> '2'
036300010330     C* ... restituisco esito OK.
036400010330     C                   eval      wrkesito = '0'
036500010330     C                   else
036600040119     C                   if        §ctrokvb > 0
036700010330     C                   eval      wrkesito = '1'
036800010330     C                   else
036900010615     C                   if        flgOk = '0'
037000010615     C                   eval      wrkesito = '2'
037100010615     C                   else
037200010615     C                   eval      wrkesito = '6'
037300010615     C                   endif
037400010330     C                   endif
037500010330     C                   endif
037600010601     C                   else
037700010601     C                   eval      wrkesito = '9'
037800010601     C                   endif
037900010330     C*
038000010330     C                   if        %open(tivin00r)
038100010330     C                   close     tivin00r
038200010330     C                   endif
038300021025     C                   if        %open(fivabwwr)
038400021025     C                   close     fivabwwr
038500010330     C                   endif
038600040512     C                   if        %open(fivatwwr)
038700040512     C                   close     fivatwwr
038800040512     C                   endif
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
039900010618     C                   if        flgGiro = '1'
040000081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
040100010618     C                   endif
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
041400070301     C                   if        wVATNOT_E <> *blanks
041500080125     C                   if        VABCTM = *blanks
041600080125     C                   eval      VABCTM = '7Q'
041700080125     C                   endif
041800070301     C                   endif
041900040119     C                   write     fivab000                                     => scarico il VAB
042000040119     C*
042100040119     C                   ENDSR
042200040119
042300040119
042400040119
042500040119     C*----------------------------------------------------*
042600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042700040119     C*----------------------------------------------------*
042800040510     C     WRIVAT        BEGSR
042900060512     C*
043000060512     C* Inizializzo i campi chiave
043100060512     C                   eval      VATFGS = VABFGS
043200060512     C                   eval      VATAAS = VABAAS
043300060512     C                   eval      VATCCM = VABCCM
043400070823     C                   eval      VATNRS = VABNRS
043500060512     C                   eval      VATNSP = VABNSP
043600060512     C                   eval      VATLNP = VABLNP
043700060331     C*
043800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043900060331     C                   if        wVATNOT_A <> *blanks
044000060331     C                   eval      VATNOT = wVATNOT_A
044100060331     C                   eval      VATTRC = 'A'
044200060331     C                   write     FIVAT000
044300060331     C                   add       1             §CTROKVT
044400060331     C                   endif
044500060331     C*
044600060331     C                   if        wVATNOT_B <> *blanks
044700060331     C                   eval      VATNOT = wVATNOT_B
044800060331     C                   eval      VATTRC = 'B'
044900060331     C                   write     FIVAT000
045000060331     C                   add       1             §CTROKVT
045100060331     C                   endif
045200060331     C*
045300080625     C                   if        wVATNOT_E <> *blanks
045400060331     C                   eval      VATNOT = wVATNOT_E
045500060331     C                   eval      VATTRC = 'E'
045600080716     C                   write     FIVAT000
045700080716     C                   add       1             §CTROKVT
045800080625     C                   endif
045900040119     C*
046000040119     C                   ENDSR
046100040119
046200040119
046300040119
046400040119     C*----------------------------------------------------*
046500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046600040119     C*----------------------------------------------------*
046700040119     C     PREVAT        BEGSR
046800040119     C*
046900040119     C* Compongo il nome del membro da dare al FIVATWWR
047000040119     C                   eval      parmbr = vlrhdl
047100040119     C                   movel     'M'           parmbr
047200040510     C                   eval      parccm = vlrksc
047300040119     C                   eval      paropz = '1'
047400040119     C* Effettuo la chiamata al CLLE preposto
047500040119     C                   call(e)   'TITVVTC'
047600040119     C                   parm                    parccm
047700040119     C                   parm                    parmbr
047800040119     C                   parm                    paropz
047900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048000040119     C                   if        %error
048100040119     C                   movel     '1'           chkcall
048200040119     C                   else
048300040119     C                   movel     '0'           chkcall
048400040119     C                   endif
048500040119     C*
048600040119     C                   ENDSR
048700010601
048800010601
048900010601
049000010330     C*----------------------------------------------------*
049100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049200010330     C*----------------------------------------------------*
049300040119     C     INZVAR        BEGSR
049400010330     C*
049500030822     C* Inizializzo variabili di wrk
049600020204     C                   Z-ADD     *zeros        Num5_0            5 0
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
053700030822     C*
053800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053900030822     C                   EVAL      posDaDft = 1
054000030822     C                   EVAL      posADft  = 0
054100030822     C                   EVAL      wGiroDft = 0
054200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054300030822     C                             posDaDft > 0
054400030822     C*
054500030822     C* Gestisco il 1° giro
054600030822     C                   IF        wGiroDft = 0
054700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054900030822     C* Incremento il contatore dei "giri"
055000030822     C                   EVAL      wGiroDft = 1
055100030822     C                   ELSE
055200030822     C                   EVAL      posDaDft = posADft
055300030822     C                   ENDIF
055400030822     C* Eseguo lo scan x trovare la fine del campo corrente
055500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055600030822     C*
055700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055900030822     C                   IF        posDaDft > 0 AND
056000030822     C                             posADft  > 0
056100030822     C* NCL
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'NCL'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiInt=*on
057100030822     C                   Z-ADD     PiVal         VABNCL
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400030822     C* CCM
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'CCM'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiInt=*on
058400030822     C                   Z-ADD     PiVal         VABCCM
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* LNP
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'LNP'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABLNP
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* NRS
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'NRS'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABNRS
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* CTR
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'CTR'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABCTR
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* PKB
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'PKB'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiNum=*on
063600030822     C                   Z-ADD     PiVal         VABPKB
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* VLB
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'VLB'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiNum=*on
064900030822     C                   Z-ADD     PiVal         VABVLB
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* QFT
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'QFT'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiNum=*on
066200030822     C                   Z-ADD     PiVal         VABQFT
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* CBO
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'CBO'
067000030822     C                   EVAL      VABCBO=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400030822     C* TSP
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'TSP'
067900030822     C                   EVAL      VABTSP=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300030822     C* VAS
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'VAS'
068800030822     C                   EVAL      VABVAS=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200030822     C* VCA
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'VCA'
069700030822     C                   EVAL      VABVCA=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100070925     C* TIC
070200070925     C                   IF        %subst(
070300070925     C                             %subst(vlrppt:posDaDft+1:
070400070925     C                             posADft-posDaDft-1):1:3)
070500070925     C                             = 'TIC'
070600070925     C                   EVAL      VABTIC=%trim(%subst(
070700070925     C                             %subst(vlrppt:posDaDft+1:
070800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900070925     C                   ENDIF
071000030822     C* GCA
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'GCA'
071500030822     C                   EVAL      VABGCA=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* CTM
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'CTM'
072400030822     C                   EVAL      VABCTM=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* FFD
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'FFD'
073300030822     C                   EVAL      VABFFD=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* VAD
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'VAD'
074200030822     C                   EVAL      VABVAD=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* GMA
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'GMA'
075100030822     C                   EVAL      VABGMA=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* GGA
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'GGA'
076000030822     C                   EVAL      VABGGA=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* GVA
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'GVA'
076900030822     C                   EVAL      VABGVA=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300030822     C* TC1
077400030822     C                   IF        %subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):1:3)
077700030822     C                             = 'TC1'
077800030822     C                   EVAL      VABTC1=%trim(%subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100030822     C                   ENDIF
078200030822     C* TC2
078300030822     C                   IF        %subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):1:3)
078600030822     C                             = 'TC2'
078700030822     C                   EVAL      VABTC2=%trim(%subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000030822     C                   ENDIF
079100040714     C* VATTRC
079200040714     C                   IF        %subst(
079300040714     C                             %subst(vlrppt:posDaDft+1:
079400040714     C                             posADft-posDaDft-1):1:3)
079500040714     C                             = 'TRC'
079600040714     C                   EVAL      VATTRC=%trim(%subst(
079700040714     C                             %subst(vlrppt:posDaDft+1:
079800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079900040714     C                   ENDIF
080000030822     C* ...
080100030822     C                   ENDIF
080200030822     C                   ENDDO
080300020204     C*
080400010330     C                   ENDSR
080500010607     C*----------------------------------------------------*
080600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080700010607     C*----------------------------------------------------*
080800010607     C     IMPVAB        BEGSR
080900010607     C*
081000010607     C                   SETOFF                                       3132
081100030515     C*
081200010607     C                   MOVEL     datcor        VABAAS
081300010607     C                   MOVE      datcor        VABMGS
081400010607     C*
081500030715     C**********
081600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081700030715     C**********
081800030715     C                   IF        wGiro = *zeros
081900080716     C*
082000080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
082100080716     C                   EVAL      depvinDTA = vinDTA
082200040113     C*
082300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082500040510     C                             %trim(intcol(3))<>*blanks
082600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082700040113     C                                      %trim(intcol(3))
082800070823     C     ';':CharCSV   XLATE     vindta        vindta
082900040510     C                   ENDIF
083000080626     C*
083100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083200031201     C     minu:maiu     XLATE     vindta        vindta
083300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083400030715     C                   DOW       posDa <= %len(%trim(vindta))
083500030715     C*
083600030715     C* Gestisco il 1° campo
083700030715     C                   IF        i = 1
083800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083900030715     C                   EVAL      posDa = 1
084000030715     C* Eseguo lo scan x trovare la fine del primo campo
084100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C* X i campi successivi al 1°
084600030715     C                   ELSE
084700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084800030715     C                   EVAL      posDa = posA + 1
084900030715     C* Eseguo lo scan x trovare la fine del campo corrente
085000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085100030715     C* Gestisco l'ultimo campo
085200030715     C                   IF        posA = *zeros
085300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085400030715     C* A questo punto "estraggo" il campo corrente
085500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085600030715     C                                                    (posA-posDa))
085700030715     C                   LEAVE
085800030715     C                   ELSE
085900030715     C* A questo punto "estraggo" il campo corrente
086000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086100030715     C                                                    (posA-posDa))
086200030715     C                   ENDIF
086300030715     C                   ENDIF
086400030715     C* Incremento il contatore d campo
086500030715     C                   EVAL      i = i +1
086600030715     C                   ENDDO
086700030715     C*
086800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086900030715     C* delimitazione testo
087000030715     C                   EVAL      i = 1
087100030715     C                   DOW       i < %elem(SkSplitFLD)
087200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087400030715     C                   EVAL      i = i + 1
087500030715     C                   ENDDO
087600030715     C                   EVAL      wGiro = 1
087700080716     C* Ripristino la 1° riga originale
087800080716     C                   EVAL      vinDTA = depvinDTA
087900030715     C                   ELSE
088000030715     C                   EVAL      wGiro = 2
088100031204     C**********
088200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088300031204     C* d separatore campo
088400031204     C**********
088500031204     C                   EVAL      vindta = %trim(vindta)
088600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088700031204     C                   EVAL      vindta = %subst(vindta:2)
088800031204     C                   ENDDO
088900031204     C*
089000031204     C                   Z-ADD     *zeros        lunghInput        4 0
089100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089500031204     C                   ENDDO
089600030715     C**********
089700030715     C* Effettuo lo split del campo dati d input
089800030715     C**********
089900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090000030715     C                   DOW       posDa <= %len(%trim(vindta))
090100030715     C*
090200030715     C* Gestisco il 1° campo
090300030715     C                   IF        i = 1
090400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090500030715     C                   EVAL      posDa = 1
090600030715     C* Eseguo lo scan x trovare la fine del primo campo
090700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090800030715     C* A questo punto "estraggo" il campo corrente
090900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091000030715     C                                                    (posA-posDa))
091100030715     C* X i campi successivi al 1°
091200030715     C                   ELSE
091300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091400030715     C                   EVAL      posDa = posA + 1
091500031204     C* Verifico che nn vi sia il campo nn valorizzato
091600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091700031204     C* Se campo nn valorizzato skippo al prossimo
091800031204     C                   EVAL      SkSplitCSV(i) = *blanks
091900031204     C                   EVAL      posA  = posA + 1
092000031204     C                   ELSE
092100030715     C* Eseguo lo scan x trovare la fine del campo corrente
092200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092300030715     C* Gestisco l'ultimo campo
092400030715     C                   IF        posA = *zeros
092500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092600030715     C* A questo punto "estraggo" il campo corrente
092700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092800030715     C                                                    (posA-posDa))
092900030715     C                   LEAVE
093000030715     C                   ELSE
093100030715     C* A questo punto "estraggo" il campo corrente
093200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093300030715     C                                                    (posA-posDa))
093400030715     C                   ENDIF
093500031204     C                   ENDIF
093600030715     C                   ENDIF
093700030715     C* Incremento il contatore d campo
093800030715     C                   EVAL      i = i +1
093900030715     C                   ENDDO
094000030715     C*
094100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094200030715     C* delimitazione testo
094300030715     C                   EVAL      i = 1
094400030715     C                   DOW       i < %elem(SkSplitCSV)
094500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094700030715     C                   EVAL      i = i + 1
094800030715     C                   ENDDO
094900030715     C*
095000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095100030715     C                   EVAL      i = 1
095200030715     C                   DOW       i < %elem(SkSplitFLD)
095300030715     C*
095400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095600030715     C                             SkSplitCSV(i) <> *blanks
095700030721     C*** ATB
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095900030721     C                   EVAL      VABATB = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030715     C*** RSD
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096600030715     C* ==
096700030715     C                   ENDIF
096800030721     C*** RD2
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030715     C*** IND
097300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097400030715     C                   EVAL      VABIND = SkSplitCSV(i)
097500030715     C                   ENDIF
097600030715     C*** LOD
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** PRD
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030721     C*** NZD
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030715     C*** RMA
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** NOT
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** NT2
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** VCA
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** VAS
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** FFD
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030721     C*** GC1
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** GC2
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TSP
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102300030721     C                   ENDIF
102400040510     C*** CBO
102500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102700040510     C                   ENDIF
102800030721     C*** NAS
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** TIC
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** GCA
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** XCO
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** CTM
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** TCR
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** CTS
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** FTM
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** VAD
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** GMA
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GGA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GVA
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** TC1
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TC2
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** SCL
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** RMO
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** NMO
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*
109700030721     C* Reperisco quindi i campi numerici...
109800040510     C*** NSP
109900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110000040510     C                   EVAL      PiStr=SkSplitCSV(i)
110100040510     C                   EXSR      CHKNUM
110200040510     C                   IF        PiInt=*on
110300040510     C                   Z-ADD     PiVal         VABNSP
110400040510     C                   ELSE
110500040510     C                   SETON                                        31
110600040510     C                   EVAL      VABNSP = *zeros
110700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110800040510     C                             + ' ' + 'VABNSP'
110900040510     C                   ENDIF
111000040510     C                   ENDIF
111100071106     C*** RMN
111200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111300071106     C                   EVAL      PiStr=SkSplitCSV(i)
111400071106     C                   EXSR      CHKNUM
111500071106     C                   IF        PiInt=*on
111600071106     C                   Z-ADD     PiVal         VABRMN
111700071106     C                   ELSE
111800071106     C                   SETON                                        32
111900071106     C                   EVAL      VABRMN = *zeros
112000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112100071106     C                             + ' ' + 'VABRMN'
112200071106     C                   ENDIF
112300071106     C                   ENDIF
112400030721     C*** CAD
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on
112900030721     C                   Z-ADD     PiVal         Num5_0
113000030721     C                   MOVEL(p)  Num5_0        VABCAD
113100030721     C                   ELSE
113200090603     C                   EVAL      VABCAD = SkSplitCSV(i)
113300090603     C                   SETON                                        32
113400090603     C                   EVAL      VABCAD = *zeros
113500090603     C                   EVAL      vinmsg = %trimr(vinmsg)
113600090603     C                             + ' ' + 'VABCAD'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** NCL
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on
114400030721     C                   Z-ADD     PiVal         VABNCL
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      VABNCL = *zeros
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900040122     C                             + ' ' + 'VABNCL'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** PKB
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115400030721     C                   EVAL      PiStr=SkSplitCSV(i)
115500030721     C                   EXSR      CHKNUM
115600030721     C                   IF        PiNum=*on
115700030721     C                   Z-ADD     PiVal         VABPKB
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000030721     C                   EVAL      VABPKB = *zeros
116100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116200030721     C                             + ' ' + 'VABPKB'
116300030721     C                   ENDIF
116400030721     C                   ENDIF
116500030721     C*** CAS
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116700080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
116800080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
116900081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
117000081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
117100080125     C                   EVAL      wFlgCAS = '1'
117200080125     C                   ENDIF
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiNum=*on
117600030721     C                   Z-ADD     PiVal         VABCAS
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABCAS = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABCAS'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** CCM
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on      AND
118900030721     C                             PiVal<=9999999 AND
119000030721     C                             PiVal>*zeros
119100030721     C                   Z-ADD     PiVal         VABCCM
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABCCM = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600040510     C                             + ' ' + 'VABCCM'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** LNP
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on    AND
120400030721     C                             PiVal<=999   AND
120500030721     C                             PiVal>*zeros
120600030721     C                   Z-ADD     PiVal         VABLNP
120700030721     C                   ELSE
120800040715     C                   SETON                                        32
120900030721     C                   EVAL      VABLNP = *zeros
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100040510     C                             + ' ' + 'VABLNP'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** NRS
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on AND
121900030721     C                             PiVal<=99
122000030721     C                   Z-ADD     PiVal         VABNRS
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABNRS'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** LNA
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on    AND
123200030721     C                             PiVal<=999   AND
123300030721     C                             PiVal>*zeros
123400030721     C                   Z-ADD     PiVal         VABLNA
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      VABLNA = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABLNA'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** CTR
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on  AND
124700030721     C                             PiVal<=999
124800030721     C                   Z-ADD     PiVal         VABCTR
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABCTR'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** IAS
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiNum=*on
126000030721     C                   Z-ADD     PiVal         VABIAS
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABIAS'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** VLB
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiNum=*on
127200030721     C                   Z-ADD     PiVal         VABVLB
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABVLB'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** QFT
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiNum=*on
128400030721     C                   Z-ADD     PiVal         VABQFT
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABQFT'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** NCD
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiInt=*on      AND
129600030721     C                             PiVal<=9999999
129700030721     C                   Z-ADD     PiVal         VABNCD
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABNCD'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** NCA
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on      AND
130900030721     C                             PiVal<=9999999
131000030721     C                   Z-ADD     PiVal         VABNCA
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABNCA'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** ZNC
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on AND
132200030721     C                             PiVal<=99
132300030721     C                   Z-ADD     PiVal         VABZNC
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABZNC'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** DCR
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on       AND
133500030721     C                             PiVal<=99999999
133600030721     C                   Z-ADD     PiVal         VABDCR
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABDCR'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** HCR
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on   AND
134800030721     C                             PiVal<=9999
134900030721     C                   Z-ADD     PiVal         VABHCR
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABHCR'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** VMD
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiNum=*on
136100030721     C                   Z-ADD     PiVal         VABVMD
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABVMD'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** ANT
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on        AND
137300030721     C                             PiVal<=999999999
137400030721     C                   Z-ADD     PiVal         VABANT
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABANT'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** CMO
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiInt=*on
138600030721     C                   Z-ADD     PiVal         Num5_0
138700030721     C                   MOVEL(p)  Num5_0        VABCMO
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      VABCMO = *zeros
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200030721     C                             + ' ' + 'VABCMO'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500040510     C*
139600040510     C* Reperisco i campi del FIVAT
139700040510     C*** VATNOT
139800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140000040510     C                   ENDIF
140100040510     C*** VATTRC
140200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140400040510     C                   ENDIF
140500060331     C*** VATNOT_A
140600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140800060331     C                   ENDIF
140900060331     C*** VATNOT_B
141000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141200060331     C                   ENDIF
141300060331     C*** VATNOT_E
141400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141600060331     C                   ENDIF
141700070823     C*
141800071029     C***  ===>  Gestione campi "particolari" dentro ciclo
141900090421     C*
142000090603     C*** W_VABRMA
142100090603     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
142200090603     C                   EVAL      VABRMA = %trim(%subst(SkSplitCSV(i):9))
142300090603     C                   ENDIF
142400090610     C*** W_VABCAD
142500090610     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
142600090610     C                   EVAL      VABCAD = %trim(SkSplitCSV(i))
142700090610     C                   ENDIF
142800070925     C*
142900071029     C***  <===  -----------------------------------------
143000040510     C*
143100030715     C                   ENDIF
143200030715     C                   EVAL      i = i + 1
143300030715     C                   ENDDO
143400070925     C*
143500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
143600090709     C*
143700090709     C* Stacco numeratore Bartolini da AZNUM x numero spedizione
143800090709     C                   clear                   TRUL33DS
143900090709     C                   eval      I33OPE = *zeros
144000090709     C                   eval      I33CNU = 302
144100090709     C                   eval      I33NUM = 1
144200090709     C                   movel     TRUL33DS      KPJBU
144300090709     C                   call      'TRUL33R'
144400090709     C                   parm                    KPJBA
144500090709     C                   movel     KPJBU         TRUL33DS
144600090709     C                   if        O33ERR = *zeros
144700090709     C                   z-add     O33NRF        VABNSP
144800090709     C                   z-add     O33NRF        VATNSP
144900090709     C                   else
145000090709     C                   SETON                                        31
145100090709     C                   EVAL      vinmsg = %trimr(vinmsg)
145200090709     C                             + ' ' + 'VABNSP VATNSP'
145300090709     C                   endif
145400080125     C*
145500090603     C* VABCTR <=> VABCTM
145600090603     C                   SELECT
145700090603     C                   WHEN      VABCTR = 300
145800090603     C                   EVAL      VABCTM = '7R'
145900090603     C                   WHEN      VABCTR = 333
146000090603     C                   EVAL      VABCTM = '7Q'
146100090603     C                   WHEN      VABCTR = 000
146200090603     C                   EVAL      VABCTM = '7Q'
146300090603     C                   WHEN      VABCTR = 090
146400090603     C                   EVAL      VABCTM = '7Q'
146500090603     C                   ENDSL
146600070925     C*
146700071029     C***  <===  -----------------------------------------
146800070925     C*
146900070925     C* Considerazioni finali su CBO/CAS
147000080125     C                   IF        wFlgCAS = '1'
147100070925     C                   IF        VABCBO = '1'
147200070925     C                   EVAL      VABCBO = '4'
147300070925     C                   ENDIF
147400070925     C                   IF        VABCBO = '2'
147500070925     C                   EVAL      VABCBO = '6'
147600070925     C                   ENDIF
147700070925     C                   ENDIF
147800070925     C***  <===  ----------------------------
147900040510     C*
148000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148100040510     C                   IF        VABNSP = *zeros
148200040510     C                   SETON                                        31
148300040510     C                   ENDIF
148400040715     C*
148500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148600040715     C                   IF        VABLNP = *zeros
148700040715     C                   SETON                                        31
148800040715     C                   ENDIF
148900040510     C*
149000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149100040510     C                   IF        VABRMN = *zeros
149200040510     C                   Z-ADD     VABNSP        VABRMN
149300040510     C                   ENDIF
149400040510     C*
149500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149600040510     C                   IF        VABRMA = *blanks
149700040714     C                   MOVEL     VABRMN        VABRMA
149800040510     C                   ENDIF
149900040128     C*
150000040510     C* Se provincia nn valorizzata la reperisco
150100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150200040510     C                   IF        VABPRD = *blanks
150300040128     C                   CLEAR                   TISI95DS
150400040128     C                   EVAL      I95TCN = '3'
150500040128     C                   Z-ADD     datcor        I95DAT
150600040128     C                   EVAL      I95NAR = VABNZD
150700040128     C                   EVAL      I95CAP = VABCAD
150800040128     C                   EVAL      I95LOC = VABLOD
150900040128     C                   CALL      'TISI95R'
151000040128     C                   PARM                    TISI95DS
151100040128     C                   EVAL      VABPRD = O95PRV
151200040510     C                   ENDIF
151300020204     C*
151400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151500020204     C                   EXSR      CHKIMPDIV
151600030715     C                   ENDIF
151700020204     C*
151800010607     C                   ENDSR
151900010607     C*----------------------------------------------------*
152000010601
152100020204
152200020204     C*----------------------------------------------------*
152300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152400020204     C*----------------------------------------------------*
152500020204     C     CHKIMPDIV     BEGSR
152600020204     C*
152700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152800020204     C                   Z-ADD     *zeros        wrkDec            9 9
152900020204     C*
153000020204     C* Come prima cosa effettuo considerazioni sulla divisa
153100020204     C                   IF        vabIAS > *zeros
153200020204     C                   IF        vabVAS <> 'EUR'
153300020204     C                   EVAL      vabVAS =  'ITL'
153400020204     C                   ENDIF
153500020204     C                   ENDIF
153600020204     C*
153700020204     C                   IF        vabCAS > *zeros
153800020204     C                   IF        vabVCA <> 'EUR'
153900020204     C                   EVAL      vabVCA =  'ITL'
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C                   IF        vabVMD > *zeros
154400020204     C                   IF        vabVAD <> 'EUR'
154500020204     C                   EVAL      vabVAD =  'ITL'
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155000020204     C                   Z-ADD     vabIAS        wrkDec
155100020204     C                   IF        wrkDec > *zeros
155200020204     C                   IF        vabVAS = 'ITL'
155300020204     C                   EVAL      vabIAS = *zeros
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155800020204     C                   Z-ADD     vabCAS        wrkDec
155900020204     C                   IF        wrkDec > *zeros
156000020204     C                   IF        vabVCA = 'ITL'
156100020204     C                   EVAL      vabCAS = *zeros
156200020204     C                   ENDIF
156300020204     C                   ENDIF
156400020204     C*
156500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156600020204     C                   Z-ADD     vabVMD        wrkDec
156700020204     C                   IF        wrkDec > *zeros
156800020204     C                   IF        vabVAD = 'ITL'
156900020204     C                   EVAL      vabVMD = *zeros
157000020204     C                   ENDIF
157100020204     C                   ENDIF
157200020204     C*
157300020204     C                   ENDSR
157400020204     C***
157500080617
157600080617
157700080617
157800080617     C*----------------------------------------------------*
157900080617     C*  CONTROLLO NUMERICITA' CAMPI
158000080617     C*----------------------------------------------------*
158100080617     C     CHKNUM        BEGSR
158200080617     C*
158300080617     C                   IF        PiDecChr = *blanks
158400080617     C                   EVAL      PiDecChr = CharNUM
158500080617     C                   ENDIF
158600080617     C*
158700080617     C                   callp     UBISNUM_Check(PiStr
158800080617     C                                          :PiDecChr
158900080617     C                                          :PiVal
159000080617     C                                          :PiNum
159100080617     C                                          :PiInt)
159200080617     C*
159300080617     C                   ENDSR
159400080617     C***
159500010330
159600010601
159700010601
159800010601
159900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160000010601     C     repfil        BEGSR
160100010601     C*
160200010601     C                   if        invfil = *zeros and
160300010601     C                             depfil > *zeros and
160400010629     C                             (vinflg = *blanks or
160500010629     C                              vinflg = *zeros)
160600010601     C
160700010601     C                   eval      invfil = depfil
160800010601     C                   endif
160900010601     C*
161000010601     C                   if        depfil <> invfil and
161100010601     C                             invfil > *zeros
161200010601     C                   eval      flgMulti = '1'
161300010601     C                   if        vinflg = *blanks
161400010601     C                   add       1             cntNonEl
161500010601     C                   endif
161600010601     C                   endif
161700010601     C*
161800010601     C                   if        vinflg = '2'
161900010601     C                   eval      flgStato = '2'
162000010601     C                   endif
162100010601     C*
162200010601     C                   ENDSR
162300010601     C***
162400010601
162500010601
162600010601
162700010330
162800040119      /TITLE Invio dei dati al punto operativo.
162900040119     C     invio         BEGSR
163000040119     C*
163100040119     C* 1° invio FIVAT
163200040119     C                   reset                   dscmz
163300040119     C                   move      vlrpoi        cmzdst
163400040119     C                   eval      cmzfld = 'FIVATWWR'
163500040119     C                   eval      cmzmbd = vlrhdl
163600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163700040119     C***                if        prmfir = *blanks
163800040119     C                   eval      cmzfla = 'FIVAT00F'
163900040119     C                   eval      cmzmba = 'FIVAT00F'
164000040119     C***                else
164100040119     C***                eval      cmzfla = prmfir
164200040119     C***                eval      cmzmba = prmfir
164300040119     C***                endif
164400040119     C                   eval      cmznrr = *zeros
164500040119     C                   move      §ctrokvt      cmznrr
164600040119     C                   eval      cmzlba = vlrfl1
164700040119     C                   call(e)   'TIS711C'
164800040119     C                   parm                    dscmz
164900040119     C                   parm      *blanks       esito
165000040119     C                   if        %error
165100040119     C                             or cmzerr = '1'
165200040119     C                             or esito  = '1'
165300040119     C                   eval      wrkesito = '3'
165400040119     C                   else
165500040119     C*
165600040119     C* 2° invio FIVAB
165700040119     C                   reset                   dscmz
165800040119     C                   move      vlrpoi        cmzdst
165900040119     C                   eval      cmzfld = vlrfou
166000040119     C                   eval      cmzmbd = vlrhdl
166100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166200040119     C***                if        prmfir = *blanks
166300040119     C                   eval      cmzfla = 'FIVAB00F'
166400040119     C                   eval      cmzmba = 'FIVAB00F'
166500040119     C***                else
166600040119     C***                eval      cmzfla = prmfir
166700040119     C***                eval      cmzmba = prmfir
166800040119     C***                endif
166900040119     C                   eval      cmznrr = *zeros
167000040119     C                   move      §ctrokvb      cmznrr
167100040119     C                   eval      cmzlba = vlrfl1
167200040119     C                   call(e)   'TIS711C'
167300040119     C                   parm                    dscmz
167400040119     C                   parm      *blanks       esito
167500040119     C                   if        %error
167600040119     C                             or cmzerr = '1'
167700040119     C                             or esito  = '1'
167800040119     C                   eval      wrkesito = '3'
167900040119     C                   endif
168000040119     C                   endif
168100040119     C*
168200040119     C                   ENDSR
168300040119     C***
168400010601
168500010601
168600010601
168700010601
168800010601      /TITLE Invio dei dati al punto operativo.
168900010601     C     opeini        BEGSR
169000010601     C*
169100010601     C* Inizializzo flag e contatori operativi
169200010601     C                   movel     '0'           flgGiro           1
169300010601     C                   movel     '0'           flgMulti          1
169400010601     C                   movel     '1'           flgStato          1
169500010615     C                   movel     '0'           flgOk             1
169600010601     C                   z-add     *zeros        cntNonEl         10 0
169700010601     C                   z-add     *zeros        depfil            3 0
169800010601     C                   z-add     *zeros        invfil            3 0
169900010601     C*
170000010601     C                   ENDSR
170100010601     C***
170200070326
170300070326
170400070326
170500070326
170600080916     C     *pssr         BEGSR
170700070329     C*
170800080916     C                   if        %open(tivin00r)
170900080916     C                   close     tivin00r
171000080916     C                   endif
171100080916     C                   if        %open(fivabwwr)
171200080916     C                   close     fivabwwr
171300080916     C                   endif
171400080916     C                   if        %open(fivatwwr)
171500080916     C                   close     fivatwwr
171600080916     C                   endif
171700070326     C*
171800070326     C* Effettuo la chiamata al CLLE preposto
171900080916     C                   call(e)   'TITVVTC'
172000080916     C                   parm                    parccm
172100080916     C                   parm                    parmbr
172200080916     C                   parm      '2'           paropz
172300070326     C*
172400080916     C                   eval      wrkesito = '2'
172500070404     C*
172600080916     C                   seton                                        LR
172700070326     C*
172800080916     C                   ENDSR     '*CANCL'
172900070326     C***
173000070326
173100070326
173200010330
173300010330
173400000613     C     *inzsr        BEGSR
173500990910     C*
173600990910     C     *entry        plist
173700990920     C                   parm                    tivlrds
173800990921     C                   parm      wrkesito      esito
173900000724     C                   parm                    prmlit
174000000710     C                   parm                    prmfir
174100010330     C*
174200010330     C* CALCOLA LA DATA CORRENTE
174300010330     C                   time                    wn14             14 0
174400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
174500010330     C                   z-add     wn8           g08dat
174600010330     C                   z-add     *zeros        g08inv
174700010330     C                   movel     '0'           g08err
174800010330     C                   call      'XSRDA8'
174900010330     C                   parm                    wlbda8
175000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
175100000613     C*
175200000613     C                   ENDSR
175300000613     C***
175400990908
175500090603     Otitv1q0p  E            riepilogo         2
175600990915     O                                              'Upload via Internet'
175700990915     O                                           +1 'Traduzione TIVIN00R -'
175800021025     O                                           +1 'FIVABWWR'
175900990915     O                                           +1 'Testate bolle'
176000990915     O          E            riepilogo   2
176100990915     O                       wrkdata
176200990915     O                       wrkora              +1
176300990915     O                       procname            +1
176400990915     O          E            riepilogo   2
176500990915     O                                              'Cliente..................:'
176600010605     O                       VABCCM        z     +1
176700990915     O          E            riepilogo   2
176800990920     O                                              'Riferimento Strategi.....:'
176900990920     O                       vlrhdl              +1
177000990915     O          E            riepilogo   2
177100990915     O                                              'Giusti...................:'
177200040119     O                       §CTROKVB      2   +  1
177300990915     O          E            riepilogo   2
177400990915     O                                              'Sbagliati e corretti.....:'
177500971022     O                       §CTRMO        2   +  1
177600990915     O          E            riepilogo   2
177700990915     O                                              'Sbagliati e scartati.....:'
177800971022     O                       §CTRNO        2   +  1
177900000613
178000090603     Otitv1q0ps E            testdett          2
178100000613     O                                              'Upload via Internet'
178200000613     O                                           +1 'Traduzione TIVIN00R -'
178300021025     O                                           +1 'FIVABWWR'
178400000613     O                                           +1 'Testate bolle'
178500000616     O          E            testdett    3
178600000613     O                                           +2 'N° rec'
178700000613     O                                           +3 'Anteprima contenuto'
178800000616     O          E            rigadett    2
178900000613     O                       rrnum               +2
179000000621     O                       recko               +3
179100000616     O          E            findett     2
179200000613     O                       wrkdata
179300000613     O                       wrkora              +1
179400000613     O                       procname            +1
179500000616     O          E            findett     2
179600000613     O                                              'Cliente..................:'
179700010605     O                       VABCCM        z     +1
179800000616     O          E            findett     2
179900000613     O                                              'Riferimento Strategi.....:'
180000000613     O                       vlrhdl              +1
180100000616     O          E            findett     2
180200000613     O                                              'Giusti...................:'
180300040119     O                       §CTROKVB      2   +  1
180400000616     O          E            findett     2
180500000613     O                                              'Sbagliati e corretti.....:'
180600000613     O                       §CTRMO        2   +  1
180700000616     O          E            findett     2
180800000613     O                                              'Sbagliati e scartati.....:'
180900000613     O                       §CTRNO        2   +  1
181000000613** CMD - COMANDI CL
181100090603OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
181200090603OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
181300090603DLTOVR FILE(TITV1Q0P TITV1Q0PS) LVL(*)
181400010606
181500000613
181600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181700090610VABCCM;VABCTR;VABRSD;VABRD2;VABIND;W_VABCAD;VABLOD;VABPRD;VABNZD;
181800090709VABNCL;VABPKB;VABRMN;W_VABRMA;VATNOT_A;VATNOT_B;VABNOT;VABNT2;
181900090603VABCAS;VABIAS;VATNOT_E;FILLER01
