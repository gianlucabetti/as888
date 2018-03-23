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
009100100908
009200100908     D*------------------
009300100908     D* DS RIDEFINIZIONE RAGIONE SOCIALE DESTINATARIO
009400100908     D*------------------
009500100908     D W_RSD           DS                  INZ
009600100908     D  W_RSD_1                      35
009700100908     D  W_RSD_2                      35
009800100908     D  W_RSD_3                      35
009900100908     D  W_RSD_4                      35
010000080617
010100080617     D*------------------
010200080617     D* LINKING A DEFINIZIONI ESTERNE
010300080617     D*------------------
010400080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010500090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010600010330
010700010330
010800010330
010900990915     C                   time                    wrkdata
011000990915     C                   time                    wrkora
011100000913     C                   reset                   rrnum
011200990921     C                   reset                   esito
011300081014     C                   reset                   wrkesito
011400010601     C*
011500010601     C                   exsr      opeini
011600010605     C                   exsr      rwvab
011700081014     C*
011800081014     C                   exsr      endela
011900010601     C*
012000010601     C                   seton                                        lr
012100010601
012200010601
012300010601
012400010601     C*--------------------------------------------------------
012500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012600010601     C*--------------------------------------------------------
012700010601     C     PREELA        BEGSR
012800010601     C*
012900010601     C* SE OCCORRE SPEDIRE IN FILIALE
013000010601     C                   if        invfil <> *zeros and
013100010601     C                             flgGiro = '0'
013200010601     C*
013300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013400010601     C                   eval      flgGiro = '1'
013500000724     C*
013600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013800010601     C     invfil        chain     azorg01l
013900000724     C                   if        %found
014000000724     C                   movel(p)  CMD(1)        depcmd
014100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
014200000724     C*
014300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014400011113     C                   Z-ADD     150           LENGH            15 5
014500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014600000724     C                   PARM                    depcmd
014700000724     C                   PARM                    LENGH
014800000724     C*
014900000724     C                   endif
015000000616     C*
015100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015200000616     C                   movel(p)  CMD(2)        depcmd
015300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015400000616     C*
015500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015600011113     C                   Z-ADD     150           LENGH            15 5
015700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015800000616     C                   PARM                    depcmd
015900000616     C                   PARM                    LENGH
016000000613     C*
016100090921     C                   if        not %open(titv1r8ps)
016200090921     C                   open      titv1r8ps
016300070329     C                   except    testdett
016400000613     C                   endif
016500010601     C*
016600010601     C                   endif
016700010601     C*
016800010601     C                   ENDSR
016900010601     C***
017000010601
017100010601
017200010601
017300010601
017400010601     C*--------------------------------------------------------
017500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017600010601     C*--------------------------------------------------------
017700010601     C     ENDELA        BEGSR
017800081014     C*
017900081014     C                   if        %open(tivin00r)
018000081014     C                   close     tivin00r
018100081014     C                   endif
018200081014     C                   if        %open(fivabwwr)
018300081014     C                   close     fivabwwr
018400081014     C                   endif
018500081014     C                   if        %open(fivatwwr)
018600081014     C                   close     fivatwwr
018700081014     C                   endif
018800040119     C*
018900040119     C* Effettuo la chiamata al CLLE preposto
019000040119     C                   call(e)   'TITVVTC'
019100040510     C                   parm                    parccm
019200040119     C                   parm                    parmbr
019300040119     C                   parm      '2'           paropz
019400050201     C*
019500050201     C* Effettuo lancio TISI95 solo x chiusura
019600050201     C                   CLEAR                   TISI95DS
019700050201     C                   EVAL      I95TLA = 'C'
019800050201     C                   CALL      'TISI95R'
019900050201     C                   PARM                    TISI95DS
020000000613     C*
020100090921     C                   if        %open(titv1r8ps)
020200000616     C                   except    findett
020300090921     C                   close     titv1r8ps
020400000613     C                   endif
020500000616     C*
020600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020700011113     C                   Z-ADD     150           LENGH            15 5
020800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020900000616     C                   PARM                    CMD(3)
021000000616     C                   PARM                    LENGH
021100000616     C*
021200010601     C                   ENDSR
021300010601     C***
021400010601
021500010601
021600010601
021700000613
021800000613     C*--------------------------------------------------------
021900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
022000000613     C*--------------------------------------------------------
022100000621     C     STPR          BEGSR
022200000613     C*
022300090921     C                   if        not %open(titv1r8p)
022400090921     C   50              open      titv1r8p
022500990915     C                   endif
022600990915     C*
022700030715     C   50              except    riepilogo
022800990915     C*
022900090921     C                   if        %open(titv1r8p)
023000090921     C   50              close     titv1r8p
023100990914     C                   endif
023200000613     C*
023300000613     C                   ENDSR
023400000613     C***
023500000613
023600010601
023700010601
023800010330     C*--------------------------------------------------------
023900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
024000010330     C*--------------------------------------------------------
024100010605     C     RWVAB         BEGSR
024200010330     C*
024300010330     C                   if        not %open(tivin00r)
024400010330     C                   open      tivin00r
024500010330     C                   endif
024600021025     C                   if        not %open(fivabwwr)
024700021025     C                   open      fivabwwr
024800010330     C                   endif
024900040119     C*
025000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
025100040119     C                   exsr      prevat
025200040119     C*
025300040119     C                   if        chkcall = '0'
025400040119     C*
025500040119     C                   if        not %open(fivatwwr)
025600040119     C                   open      fivatwwr
025700040119     C                   endif
025800010330     C*
025900040119     C                   clear                   §CTROKVB          7 0
026000040119     C                   clear                   §CTROKVT          7 0
026100040119     C                   clear                   §CTRMO            7 0
026200040119     C                   clear                   §CTRNO            7 0
026300070823     C*
026400070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
026500070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
026600090420     C                   exsr      inzvar
026700090420     C                   exsr      impvab
026800010330     C*
026900030822     C                   do        *HIVAL
027000010330     C*
027100030822     C                   read      tivin00r                               70
027200010618     C*
027300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027400010618     C                   if        vinflg = '1'
027500010618     C                   eval      flgOk = '1'
027600010618     C                   endif
027700040510     C*
027800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027900040510     C                   z-add     *zeros        wLenVINDTA        4 0
028000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
028100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
028200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
028300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
028400040510     C                   z-add     1             wFlgVINDTA
028500040510     C                   leave
028600040510     C                   endif
028700040510     C                   enddo
028800090420     C*
028900090420     C                   add       1             rrnum
029000010618     C*
029100040510     C                   if        vindta > *blanks AND
029200090420     C                             wFlgVINDTA = 1
029300010330     C*
029400010601     C                   if        *in70 = *off and
029500010330     C                             (vinflg = *blanks
029600010330     C                              or vinflg = '0'
029700010330     C                              or vinflg = '2')
029800010330     C*
029900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
030000010711     C                   if        vinflg = *blanks or vinflg = '0'
030100010711     C                   clear                   vinmsg
030200010711     C                   endif
030300010601     C*
030400090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
030500090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
030600090421     C                   eval      vindta  = '*' + %trim(vindta)
030700090421     C                   endif
030800070925     C*
030900010330     C                   exsr      inzvar
031000010605     C                   exsr      impvab
031100010601     C*
031200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
031300010605     C                   eval      depfil = VABLNP
031400010601     C                   exsr      repfil
031500010601     C                   if        depfil = invfil
031600021025     C                   if        vlrpoi = 999
031700030822     C                   move(P)   invfil        VABFGS
031800021025     C                   else
031900030822     C                   move(P)   vlrpoi        VABFGS
032000021025     C                   endif
032100010601     C*
032200010601     C                   exsr      PREELA
032300010601     C*
032400010604     C* Ebbene...
032500010604     C*
032600040119     C  N31              add       1             §CTROKVB
032700040119     C   32              add       1             §CTRMO
032800040119     C   31              add       1             §CTRNO
032900030822     C                   if        wGiro = 2
033000040510     C* Scrivo sempre il VAT
033100040510     C  N31              exsr      wrivat
033200040510     C* Scrivo il VAB solo a rottura d spedizione
033300040119     C                   if        depspe =  *zeros OR
033400040119     C                             depspe <> vabNSP
033500040119     C  N31              exsr      wrivab
033600040119     C  N31              eval      depspe = vabNSP
033700040119     C                   endif
033800040119     C                   endif
033900010604     C*
034000010604     C                   if        *in31 = *off and
034100010604     C                             *in32 = *off
034200010604     C                   eval      vinflg = '1'
034300010604     C                   else
034400010604     C                   eval      recko = vindta
034500010604     C                   except    rigadett
034600010604     C                   eval      vinflg = '2'
034700010604     C                   endif
034800010604     C                   endif
034900010601     C*
035000010604     C                   endif
035100010604     C*
035200010330     C                   else
035300010330     C                   eval      vinflg = '1'
035400010330     C                   endif
035500010601     C*
035600010601     C  N70              update    tivin000
035700010330     C*
035800030822     C  N70              enddo
035900040119     C*
036000040119     C                   endif                                                  (endif - chkcall)
036100010601     C*
036200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
036300010601     C                   if        cntNonEl = *zeros or
036400010601     C                             flgMulti = '0'
036500010330     C* Se non ci sono record con errori ...
036600010601     C                   if        §ctrno = 0 and
036700010604     C                             §ctrmo = 0 and
036800010601     C                             flgStato <> '2'
036900010330     C* ... restituisco esito OK.
037000010330     C                   eval      wrkesito = '0'
037100010330     C                   else
037200040119     C                   if        §ctrokvb > 0
037300010330     C                   eval      wrkesito = '1'
037400010330     C                   else
037500010615     C                   if        flgOk = '0'
037600010615     C                   eval      wrkesito = '2'
037700010615     C                   else
037800010615     C                   eval      wrkesito = '6'
037900010615     C                   endif
038000010330     C                   endif
038100010330     C                   endif
038200010601     C                   else
038300010601     C                   eval      wrkesito = '9'
038400010601     C                   endif
038500010330     C*
038600010330     C                   if        %open(tivin00r)
038700010330     C                   close     tivin00r
038800010330     C                   endif
038900021025     C                   if        %open(fivabwwr)
039000021025     C                   close     fivabwwr
039100010330     C                   endif
039200040512     C                   if        %open(fivatwwr)
039300040512     C                   close     fivatwwr
039400040512     C                   endif
039500010601     C*
039600010601     C                   if        vlrpoi <> 999
039700010601     C                   eval      invfil = vlrpoi
039800010601     C                   endif
039900010330     C*
040000040119     C                   if        §ctrokvb > 0
040100010601     C                             and invfil > *zeros
040200010330     C                   exsr      invio
040300010330     C                   endif
040400010601     C*
040500010618     C                   if        flgGiro = '1'
040600081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
040700010618     C                   endif
040800010330     C*
040900010330     C                   ENDSR
041000010330     C***
041100040119
041200040119
041300040119
041400040119     C*----------------------------------------------------*
041500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
041600040119     C*----------------------------------------------------*
041700040510     C     WRIVAB        BEGSR
041800040119     C*
041900060331     C* Gestisco l'eventuale rottura x numero spedizione
042000070823     C                   if        VABNSP <> depspe
042100070301     C                   if        wVATNOT_E <> *blanks
042200080125     C                   if        VABCTM = *blanks
042300080125     C                   eval      VABCTM = '7Q'
042400080125     C                   endif
042500070301     C                   endif
042600040119     C                   write     fivab000                                     => scarico il VAB
042700060331     C                   endif
042800040119     C*
042900040119     C                   ENDSR
043000040119
043100040119
043200040119
043300040119     C*----------------------------------------------------*
043400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
043500040119     C*----------------------------------------------------*
043600040510     C     WRIVAT        BEGSR
043700060512     C*
043800060512     C* Inizializzo i campi chiave
043900060512     C                   eval      VATFGS = VABFGS
044000060512     C                   eval      VATAAS = VABAAS
044100060512     C                   eval      VATCCM = VABCCM
044200070823     C                   eval      VATNRS = VABNRS
044300060512     C                   eval      VATNSP = VABNSP
044400060512     C                   eval      VATLNP = VABLNP
044500060331     C*
044600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
044700060331     C                   if        wVATNOT_A <> *blanks
044800091001     C                   eval      VATNOT = %trim(wVATNOT_A)
044900060331     C                   eval      VATTRC = 'A'
045000060331     C                   write     FIVAT000
045100060331     C                   add       1             §CTROKVT
045200060331     C                   endif
045300060331     C*
045400060331     C                   if        wVATNOT_B <> *blanks
045500091001     C                   eval      VATNOT = %trim(wVATNOT_B)
045600060331     C                   eval      VATTRC = 'B'
045700060331     C                   write     FIVAT000
045800060331     C                   add       1             §CTROKVT
045900060331     C                   endif
046000060331     C*
046100080625     C                   if        wVATNOT_E <> *blanks
046200091001     C                   eval      VATNOT = %trim(wVATNOT_E)
046300060331     C                   eval      VATTRC = 'E'
046400080716     C                   write     FIVAT000
046500080716     C                   add       1             §CTROKVT
046600080625     C                   endif
046700040119     C*
046800040119     C                   ENDSR
046900040119
047000040119
047100040119
047200040119     C*----------------------------------------------------*
047300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047400040119     C*----------------------------------------------------*
047500040119     C     PREVAT        BEGSR
047600040119     C*
047700040119     C* Compongo il nome del membro da dare al FIVATWWR
047800040119     C                   eval      parmbr = vlrhdl
047900040119     C                   movel     'M'           parmbr
048000040510     C                   eval      parccm = vlrksc
048100040119     C                   eval      paropz = '1'
048200040119     C* Effettuo la chiamata al CLLE preposto
048300040119     C                   call(e)   'TITVVTC'
048400040119     C                   parm                    parccm
048500040119     C                   parm                    parmbr
048600040119     C                   parm                    paropz
048700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048800040119     C                   if        %error
048900040119     C                   movel     '1'           chkcall
049000040119     C                   else
049100040119     C                   movel     '0'           chkcall
049200040119     C                   endif
049300040119     C*
049400040119     C                   ENDSR
049500010601
049600010601
049700010601
049800010330     C*----------------------------------------------------*
049900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050000010330     C*----------------------------------------------------*
050100040119     C     INZVAR        BEGSR
050200010330     C*
050300030822     C* Inizializzo variabili di wrk
050400020204     C                   Z-ADD     *zeros        Num5_0            5 0
050500100908     C                   CLEAR                   W_RSD
050600060331     C                   MOVEL     *blanks       wVATNOT_A
050700060331     C                   MOVEL     *blanks       wVATNOT_B
050800060331     C                   MOVEL     *blanks       wVATNOT_E
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
054100090421     C*
054200090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
054300090421     C                   EVAL      VABCBO = '1'
054400090421     C                   EVAL      VABLNP = vlrpoi
054500090421     C                   EVAL      VABNSP = rrnum
054600091026     C*
054700091026     C*** PERSONALIZZAZIONE CLIENTE ***
054800100908     C***                MOVEL     *blanks       wNOTE            70
054900100908     C***                EVAL      wNOTE = 'RICHIESTA VARIAZIONE INDIRIZZO ' +
055000100908     C***                          'E FERMO DEPOSITO AUTORIZZATO -NO GIACENZA'
055100100908     C***                EVAL      VABNOT = %subst(wNOTE:1:35)
055200100908     C***                EVAL      VABNT2 = %subst(wNOTE:36:35)
055300091026     C*********************************
055400030822     C*
055500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055600030822     C                   EVAL      posDaDft = 1
055700030822     C                   EVAL      posADft  = 0
055800030822     C                   EVAL      wGiroDft = 0
055900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056000030822     C                             posDaDft > 0
056100030822     C*
056200030822     C* Gestisco il 1° giro
056300030822     C                   IF        wGiroDft = 0
056400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056600030822     C* Incremento il contatore dei "giri"
056700030822     C                   EVAL      wGiroDft = 1
056800030822     C                   ELSE
056900030822     C                   EVAL      posDaDft = posADft
057000030822     C                   ENDIF
057100030822     C* Eseguo lo scan x trovare la fine del campo corrente
057200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057300030822     C*
057400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057600030822     C                   IF        posDaDft > 0 AND
057700030822     C                             posADft  > 0
057800030822     C* NCL
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'NCL'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABNCL
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* CCM
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'CCM'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABCCM
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* LNP
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'LNP'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABLNP
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* NRS
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'NRS'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABNRS
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* CTR
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'CTR'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiInt=*on
064000030822     C                   Z-ADD     PiVal         VABCTR
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* PKB
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'PKB'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiNum=*on
065300030822     C                   Z-ADD     PiVal         VABPKB
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* VLB
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'VLB'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiNum=*on
066600030822     C                   Z-ADD     PiVal         VABVLB
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* QFT
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'QFT'
067400030822     C                   EVAL      PiStr=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   EXSR      CHKNUM
067800030822     C                   IF        PiNum=*on
067900030822     C                   Z-ADD     PiVal         VABQFT
068000030822     C                   ENDIF
068100030822     C                   ENDIF
068200030822     C* CBO
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'CBO'
068700030822     C                   EVAL      VABCBO=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* TSP
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'TSP'
069600030822     C                   EVAL      VABTSP=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* VAS
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'VAS'
070500030822     C                   EVAL      VABVAS=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* VCA
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'VCA'
071400030822     C                   EVAL      VABVCA=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800070925     C* TIC
071900070925     C                   IF        %subst(
072000070925     C                             %subst(vlrppt:posDaDft+1:
072100070925     C                             posADft-posDaDft-1):1:3)
072200070925     C                             = 'TIC'
072300070925     C                   EVAL      VABTIC=%trim(%subst(
072400070925     C                             %subst(vlrppt:posDaDft+1:
072500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600070925     C                   ENDIF
072700030822     C* GCA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'GCA'
073200030822     C                   EVAL      VABGCA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* CTM
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'CTM'
074100030822     C                   EVAL      VABCTM=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* FFD
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'FFD'
075000030822     C                   EVAL      VABFFD=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* VAD
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'VAD'
075900030822     C                   EVAL      VABVAD=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* GMA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GMA'
076800030822     C                   EVAL      VABGMA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* GGA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GGA'
077700030822     C                   EVAL      VABGGA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* GVA
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'GVA'
078600030822     C                   EVAL      VABGVA=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* TC1
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'TC1'
079500030822     C                   EVAL      VABTC1=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* TC2
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'TC2'
080400030822     C                   EVAL      VABTC2=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800040714     C* VATTRC
080900040714     C                   IF        %subst(
081000040714     C                             %subst(vlrppt:posDaDft+1:
081100040714     C                             posADft-posDaDft-1):1:3)
081200040714     C                             = 'TRC'
081300040714     C                   EVAL      VATTRC=%trim(%subst(
081400040714     C                             %subst(vlrppt:posDaDft+1:
081500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600040714     C                   ENDIF
081700030822     C* ...
081800030822     C                   ENDIF
081900030822     C                   ENDDO
082000020204     C*
082100010330     C                   ENDSR
082200010607     C*----------------------------------------------------*
082300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082400010607     C*----------------------------------------------------*
082500010607     C     IMPVAB        BEGSR
082600010607     C*
082700010607     C                   SETOFF                                       3132
082800030515     C*
082900010607     C                   MOVEL     datcor        VABAAS
083000010607     C                   MOVE      datcor        VABMGS
083100010607     C*
083200030715     C**********
083300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083400030715     C**********
083500030715     C                   IF        wGiro = *zeros
083600080716     C*
083700080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
083800080716     C                   EVAL      depvinDTA = vinDTA
083900040113     C*
084000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084200040510     C                             %trim(intcol(3))<>*blanks
084300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084400040113     C                                      %trim(intcol(3))
084500070823     C     ';':CharCSV   XLATE     vindta        vindta
084600040510     C                   ENDIF
084700080626     C*
084800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084900031201     C     minu:maiu     XLATE     vindta        vindta
085000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085100030715     C                   DOW       posDa <= %len(%trim(vindta))
085200030715     C*
085300030715     C* Gestisco il 1° campo
085400030715     C                   IF        i = 1
085500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085600030715     C                   EVAL      posDa = 1
085700030715     C* Eseguo lo scan x trovare la fine del primo campo
085800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085900030715     C* A questo punto "estraggo" il campo corrente
086000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086100030715     C                                                    (posA-posDa))
086200030715     C* X i campi successivi al 1°
086300030715     C                   ELSE
086400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086500030715     C                   EVAL      posDa = posA + 1
086600030715     C* Eseguo lo scan x trovare la fine del campo corrente
086700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086800030715     C* Gestisco l'ultimo campo
086900030715     C                   IF        posA = *zeros
087000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087100030715     C* A questo punto "estraggo" il campo corrente
087200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087300030715     C                                                    (posA-posDa))
087400030715     C                   LEAVE
087500030715     C                   ELSE
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C                   ENDIF
088000030715     C                   ENDIF
088100030715     C* Incremento il contatore d campo
088200030715     C                   EVAL      i = i +1
088300030715     C                   ENDDO
088400030715     C*
088500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088600030715     C* delimitazione testo
088700030715     C                   EVAL      i = 1
088800030715     C                   DOW       i < %elem(SkSplitFLD)
088900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089100030715     C                   EVAL      i = i + 1
089200030715     C                   ENDDO
089300030715     C                   EVAL      wGiro = 1
089400080716     C* Ripristino la 1° riga originale
089500080716     C                   EVAL      vinDTA = depvinDTA
089600030715     C                   ELSE
089700030715     C                   EVAL      wGiro = 2
089800031204     C**********
089900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090000031204     C* d separatore campo
090100031204     C**********
090200031204     C                   EVAL      vindta = %trim(vindta)
090300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090400031204     C                   EVAL      vindta = %subst(vindta:2)
090500031204     C                   ENDDO
090600031204     C*
090700031204     C                   Z-ADD     *zeros        lunghInput        4 0
090800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091200031204     C                   ENDDO
091300030715     C**********
091400030715     C* Effettuo lo split del campo dati d input
091500030715     C**********
091600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091700030715     C                   DOW       posDa <= %len(%trim(vindta))
091800030715     C*
091900030715     C* Gestisco il 1° campo
092000030715     C                   IF        i = 1
092100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092200030715     C                   EVAL      posDa = 1
092300030715     C* Eseguo lo scan x trovare la fine del primo campo
092400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C* X i campi successivi al 1°
092900030715     C                   ELSE
093000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093100030715     C                   EVAL      posDa = posA + 1
093200031204     C* Verifico che nn vi sia il campo nn valorizzato
093300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093400031204     C* Se campo nn valorizzato skippo al prossimo
093500031204     C                   EVAL      SkSplitCSV(i) = *blanks
093600031204     C                   EVAL      posA  = posA + 1
093700031204     C                   ELSE
093800030715     C* Eseguo lo scan x trovare la fine del campo corrente
093900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094000030715     C* Gestisco l'ultimo campo
094100030715     C                   IF        posA = *zeros
094200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C                   LEAVE
094700030715     C                   ELSE
094800030715     C* A questo punto "estraggo" il campo corrente
094900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095000030715     C                                                    (posA-posDa))
095100030715     C                   ENDIF
095200031204     C                   ENDIF
095300030715     C                   ENDIF
095400030715     C* Incremento il contatore d campo
095500030715     C                   EVAL      i = i +1
095600030715     C                   ENDDO
095700030715     C*
095800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095900030715     C* delimitazione testo
096000030715     C                   EVAL      i = 1
096100030715     C                   DOW       i < %elem(SkSplitCSV)
096200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096400030715     C                   EVAL      i = i + 1
096500030715     C                   ENDDO
096600030715     C*
096700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096800030715     C                   EVAL      i = 1
096900030715     C                   DOW       i < %elem(SkSplitFLD)
097000030715     C*
097100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097300030715     C                             SkSplitCSV(i) <> *blanks
097400030721     C*** ATB
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097600030721     C                   EVAL      VABATB = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030715     C*** RSD
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098300030715     C* ==
098400030715     C                   ENDIF
098500030721     C*** RD2
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030715     C*** IND
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099100030715     C                   EVAL      VABIND = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** LOD
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** PRD
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030721     C*** NZD
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030715     C*** RMA
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** NOT
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** NT2
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** VCA
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** VAS
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030715     C*** FFD
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030721     C*** GC1
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** GC2
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** TSP
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104000030721     C                   ENDIF
104100040510     C*** CBO
104200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104400040510     C                   ENDIF
104500030721     C*** NAS
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** TIC
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** GCA
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** XCO
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** CTM
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** TCR
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** CTS
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** FTM
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** VAD
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** GMA
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** GGA
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** GVA
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** TC1
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** TC2
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** SCL
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** RMO
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** NMO
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*
111400030721     C* Reperisco quindi i campi numerici...
111500040510     C*** NSP
111600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111700040510     C                   EVAL      PiStr=SkSplitCSV(i)
111800040510     C                   EXSR      CHKNUM
111900040510     C                   IF        PiInt=*on
112000040510     C                   Z-ADD     PiVal         VABNSP
112100040510     C                   ELSE
112200040510     C                   SETON                                        31
112300040510     C                   EVAL      VABNSP = *zeros
112400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112500040510     C                             + ' ' + 'VABNSP'
112600040510     C                   ENDIF
112700040510     C                   ENDIF
112800071106     C*** RMN
112900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113000071106     C                   EVAL      PiStr=SkSplitCSV(i)
113100071106     C                   EXSR      CHKNUM
113200071106     C                   IF        PiInt=*on
113300071106     C                   Z-ADD     PiVal         VABRMN
113400071106     C                   ELSE
113500071106     C                   SETON                                        32
113600071106     C                   EVAL      VABRMN = *zeros
113700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113800071106     C                             + ' ' + 'VABRMN'
113900071106     C                   ENDIF
114000071106     C                   ENDIF
114100030721     C*** CAD
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114300030721     C                   EVAL      PiStr=SkSplitCSV(i)
114400030721     C                   EXSR      CHKNUM
114500030721     C                   IF        PiInt=*on
114600030721     C                   Z-ADD     PiVal         Num5_0
114700030721     C                   MOVEL(p)  Num5_0        VABCAD
114800030721     C                   ELSE
114900090603     C                   EVAL      VABCAD = SkSplitCSV(i)
115000090603     C                   SETON                                        32
115100090603     C                   EVAL      VABCAD = *zeros
115200090603     C                   EVAL      vinmsg = %trimr(vinmsg)
115300090603     C                             + ' ' + 'VABCAD'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** NCL
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on
116100030721     C                   Z-ADD     PiVal         VABNCL
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABNCL = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600040122     C                             + ' ' + 'VABNCL'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** PKB
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiNum=*on
117400030721     C                   Z-ADD     PiVal         VABPKB
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      VABPKB = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900030721     C                             + ' ' + 'VABPKB'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** CAS
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118400080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
118500080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
118600081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
118700081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
118800080125     C                   EVAL      wFlgCAS = '1'
118900080125     C                   ENDIF
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiNum=*on
119300030721     C                   Z-ADD     PiVal         VABCAS
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      VABCAS = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABCAS'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** CCM
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on      AND
120600030721     C                             PiVal<=9999999 AND
120700030721     C                             PiVal>*zeros
120800030721     C                   Z-ADD     PiVal         VABCCM
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      VABCCM = *zeros
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300040510     C                             + ' ' + 'VABCCM'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** LNP
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiInt=*on    AND
122100030721     C                             PiVal<=999   AND
122200030721     C                             PiVal>*zeros
122300030721     C                   Z-ADD     PiVal         VABLNP
122400030721     C                   ELSE
122500040715     C                   SETON                                        32
122600030721     C                   EVAL      VABLNP = *zeros
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800040510     C                             + ' ' + 'VABLNP'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** NRS
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiInt=*on AND
123600030721     C                             PiVal<=99
123700030721     C                   Z-ADD     PiVal         VABNRS
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABNRS'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** LNA
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on    AND
124900030721     C                             PiVal<=999   AND
125000030721     C                             PiVal>*zeros
125100030721     C                   Z-ADD     PiVal         VABLNA
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      VABLNA = *zeros
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABLNA'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** CTR
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on  AND
126400030721     C                             PiVal<=999
126500030721     C                   Z-ADD     PiVal         VABCTR
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABCTR'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** IAS
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABIAS
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABIAS'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** VLB
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiNum=*on
128900030721     C                   Z-ADD     PiVal         VABVLB
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABVLB'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** QFT
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiNum=*on
130100030721     C                   Z-ADD     PiVal         VABQFT
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABQFT'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** NCD
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiInt=*on      AND
131300030721     C                             PiVal<=9999999
131400030721     C                   Z-ADD     PiVal         VABNCD
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABNCD'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** NCA
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on      AND
132600030721     C                             PiVal<=9999999
132700030721     C                   Z-ADD     PiVal         VABNCA
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABNCA'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** ZNC
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on AND
133900030721     C                             PiVal<=99
134000030721     C                   Z-ADD     PiVal         VABZNC
134100030721     C                   ELSE
134200030721     C                   SETON                                        32
134300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134400030721     C                             + ' ' + 'VABZNC'
134500030721     C                   ENDIF
134600030721     C                   ENDIF
134700030721     C*** DCR
134800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134900030721     C                   EVAL      PiStr=SkSplitCSV(i)
135000030721     C                   EXSR      CHKNUM
135100030721     C                   IF        PiInt=*on       AND
135200030721     C                             PiVal<=99999999
135300030721     C                   Z-ADD     PiVal         VABDCR
135400030721     C                   ELSE
135500030721     C                   SETON                                        32
135600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135700030721     C                             + ' ' + 'VABDCR'
135800030721     C                   ENDIF
135900030721     C                   ENDIF
136000030721     C*** HCR
136100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136200030721     C                   EVAL      PiStr=SkSplitCSV(i)
136300030721     C                   EXSR      CHKNUM
136400030721     C                   IF        PiInt=*on   AND
136500030721     C                             PiVal<=9999
136600030721     C                   Z-ADD     PiVal         VABHCR
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABHCR'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** VMD
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiNum=*on
137800030721     C                   Z-ADD     PiVal         VABVMD
137900030721     C                   ELSE
138000030721     C                   SETON                                        32
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200030721     C                             + ' ' + 'VABVMD'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500030721     C*** ANT
138600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138700030721     C                   EVAL      PiStr=SkSplitCSV(i)
138800030721     C                   EXSR      CHKNUM
138900030721     C                   IF        PiInt=*on        AND
139000030721     C                             PiVal<=999999999
139100030721     C                   Z-ADD     PiVal         VABANT
139200030721     C                   ELSE
139300030721     C                   SETON                                        32
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500030721     C                             + ' ' + 'VABANT'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800030721     C*** CMO
139900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140000030721     C                   EVAL      PiStr=SkSplitCSV(i)
140100030721     C                   EXSR      CHKNUM
140200030721     C                   IF        PiInt=*on
140300030721     C                   Z-ADD     PiVal         Num5_0
140400030721     C                   MOVEL(p)  Num5_0        VABCMO
140500030721     C                   ELSE
140600030721     C                   SETON                                        32
140700030721     C                   EVAL      VABCMO = *zeros
140800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140900030721     C                             + ' ' + 'VABCMO'
141000030721     C                   ENDIF
141100030721     C                   ENDIF
141200040510     C*
141300040510     C* Reperisco i campi del FIVAT
141400040510     C*** VATNOT
141500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141700040510     C                   ENDIF
141800040510     C*** VATTRC
141900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142100040510     C                   ENDIF
142200060331     C*** VATNOT_A
142300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142500060331     C                   ENDIF
142600060331     C*** VATNOT_B
142700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142900060331     C                   ENDIF
143000060331     C*** VATNOT_E
143100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143300060331     C                   ENDIF
143400070823     C*
143500071029     C***  ===>  Gestione campi "particolari" dentro ciclo
143600090421     C*
143700090921     C*** W_VABRSD
143800090622     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
143900100908     C                   MOVEL     SkSplitCSV(i) W_RSD
144000090622     C                   ENDIF
144100100112     C*** W_VATNOTA1
144200100112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTA1'
144300100112     C                   EVAL      wVATNOT_A = %trim(wVATNOT_A) + ' ' +
144400090921     C                                         %trim(SkSplitCSV(i))
144500090921     C                   ENDIF
144600100112     C*** W_VATNOTB1
144700100112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTB1'
144800090921     C                   EVAL      wVATNOT_B = %trim(wVATNOT_B) + ' ' +
144900090921     C                                         %trim(SkSplitCSV(i))
145000090921     C                   ENDIF
145100070925     C*
145200071029     C***  <===  -----------------------------------------
145300040510     C*
145400030715     C                   ENDIF
145500030715     C                   EVAL      i = i + 1
145600030715     C                   ENDDO
145700070925     C*
145800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145900090921     C*
146000090921     C*
146100090921     C* NSP => Stacco un numeratore da AZNUM
146200090921     C                   clear                   TRUL33DS
146300090921     C                   eval      I33OPE = *zeros
146400090921     C                   eval      I33CNU = 302
146500090921     C                   eval      I33NUM = 1
146600090921     C                   movel     TRUL33DS      KPJBU
146700090921     C                   call      'TRUL33R'
146800090921     C                   parm                    KPJBA
146900090921     C                   movel     KPJBU         TRUL33DS
147000090921     C                   if        O33ERR = *zeros
147100090921     C                   z-add     O33NRF        VABNSP
147200090921     C                   z-add     O33NRF        VATNSP
147300090921     C                   else
147400090921     C                   SETON                                        31
147500090921     C                   EVAL      vinmsg = %trimr(vinmsg)
147600090921     C                             + ' ' + 'VABNSP VATNSP'
147700090921     C                   endif
147800080125     C*
147900100908     C                   EVAL      VABRSD = W_RSD_1
148000100908     C                   EVAL      VABRD2 = W_RSD_2
148100100908     C                   EVAL      VABNOT = W_RSD_3
148200100908     C                   EVAL      VABNT2 = W_RSD_4
148300070925     C*
148400071029     C***  <===  -----------------------------------------
148500101220     C*
148600101220     C* Ulteriore eventuale forzatura successiva
148700101220     C                   IF        VABNOT+VABNT2 = *blanks
148800101220     C                   EVAL      VABNOT =
148900101220     C                             'VARIAZIONE INDIR. E FERMO DEP. '
149000101220     C                   EVAL      VABNT2 =
149100101220     C                             'AUTORIZZATO - NO GIACENZA'
149200101220     C                   ENDIF
149300070925     C*
149400070925     C* Considerazioni finali su CBO/CAS
149500080125     C                   IF        wFlgCAS = '1'
149600070925     C                   IF        VABCBO = '1'
149700070925     C                   EVAL      VABCBO = '4'
149800070925     C                   ENDIF
149900070925     C                   IF        VABCBO = '2'
150000070925     C                   EVAL      VABCBO = '6'
150100070925     C                   ENDIF
150200070925     C                   ENDIF
150300070925     C***  <===  ----------------------------
150400040510     C*
150500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
150600040510     C                   IF        VABNSP = *zeros
150700040510     C                   SETON                                        31
150800040510     C                   ENDIF
150900040715     C*
151000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
151100040715     C                   IF        VABLNP = *zeros
151200040715     C                   SETON                                        31
151300040715     C                   ENDIF
151400040510     C*
151500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
151600040510     C                   IF        VABRMN = *zeros
151700040510     C                   Z-ADD     VABNSP        VABRMN
151800040510     C                   ENDIF
151900040510     C*
152000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
152100040510     C                   IF        VABRMA = *blanks
152200040714     C                   MOVEL     VABRMN        VABRMA
152300040510     C                   ENDIF
152400040128     C*
152500040510     C* Se provincia nn valorizzata la reperisco
152600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
152700040510     C                   IF        VABPRD = *blanks
152800040128     C                   CLEAR                   TISI95DS
152900040128     C                   EVAL      I95TCN = '3'
153000040128     C                   Z-ADD     datcor        I95DAT
153100040128     C                   EVAL      I95NAR = VABNZD
153200040128     C                   EVAL      I95CAP = VABCAD
153300040128     C                   EVAL      I95LOC = VABLOD
153400040128     C                   CALL      'TISI95R'
153500040128     C                   PARM                    TISI95DS
153600040128     C                   EVAL      VABPRD = O95PRV
153700040510     C                   ENDIF
153800020204     C*
153900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
154000020204     C                   EXSR      CHKIMPDIV
154100030715     C                   ENDIF
154200020204     C*
154300010607     C                   ENDSR
154400010607     C*----------------------------------------------------*
154500010601
154600020204
154700020204     C*----------------------------------------------------*
154800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154900020204     C*----------------------------------------------------*
155000020204     C     CHKIMPDIV     BEGSR
155100020204     C*
155200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
155300020204     C                   Z-ADD     *zeros        wrkDec            9 9
155400020204     C*
155500020204     C* Come prima cosa effettuo considerazioni sulla divisa
155600020204     C                   IF        vabIAS > *zeros
155700020204     C                   IF        vabVAS <> 'EUR'
155800020204     C                   EVAL      vabVAS =  'ITL'
155900020204     C                   ENDIF
156000020204     C                   ENDIF
156100020204     C*
156200020204     C                   IF        vabCAS > *zeros
156300020204     C                   IF        vabVCA <> 'EUR'
156400020204     C                   EVAL      vabVCA =  'ITL'
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C                   IF        vabVMD > *zeros
156900020204     C                   IF        vabVAD <> 'EUR'
157000020204     C                   EVAL      vabVAD =  'ITL'
157100020204     C                   ENDIF
157200020204     C                   ENDIF
157300020204     C*
157400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157500020204     C                   Z-ADD     vabIAS        wrkDec
157600020204     C                   IF        wrkDec > *zeros
157700020204     C                   IF        vabVAS = 'ITL'
157800020204     C                   EVAL      vabIAS = *zeros
157900020204     C                   ENDIF
158000020204     C                   ENDIF
158100020204     C*
158200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
158300020204     C                   Z-ADD     vabCAS        wrkDec
158400020204     C                   IF        wrkDec > *zeros
158500020204     C                   IF        vabVCA = 'ITL'
158600020204     C                   EVAL      vabCAS = *zeros
158700020204     C                   ENDIF
158800020204     C                   ENDIF
158900020204     C*
159000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
159100020204     C                   Z-ADD     vabVMD        wrkDec
159200020204     C                   IF        wrkDec > *zeros
159300020204     C                   IF        vabVAD = 'ITL'
159400020204     C                   EVAL      vabVMD = *zeros
159500020204     C                   ENDIF
159600020204     C                   ENDIF
159700020204     C*
159800020204     C                   ENDSR
159900020204     C***
160000080617
160100080617
160200080617
160300080617     C*----------------------------------------------------*
160400080617     C*  CONTROLLO NUMERICITA' CAMPI
160500080617     C*----------------------------------------------------*
160600080617     C     CHKNUM        BEGSR
160700080617     C*
160800080617     C                   IF        PiDecChr = *blanks
160900080617     C                   EVAL      PiDecChr = CharNUM
161000080617     C                   ENDIF
161100080617     C*
161200080617     C                   callp     UBISNUM_Check(PiStr
161300080617     C                                          :PiDecChr
161400080617     C                                          :PiVal
161500080617     C                                          :PiNum
161600080617     C                                          :PiInt)
161700080617     C*
161800080617     C                   ENDSR
161900080617     C***
162000010330
162100010601
162200010601
162300010601
162400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
162500010601     C     repfil        BEGSR
162600010601     C*
162700010601     C                   if        invfil = *zeros and
162800010601     C                             depfil > *zeros and
162900010629     C                             (vinflg = *blanks or
163000010629     C                              vinflg = *zeros)
163100010601     C
163200010601     C                   eval      invfil = depfil
163300010601     C                   endif
163400010601     C*
163500010601     C                   if        depfil <> invfil and
163600010601     C                             invfil > *zeros
163700010601     C                   eval      flgMulti = '1'
163800010601     C                   if        vinflg = *blanks
163900010601     C                   add       1             cntNonEl
164000010601     C                   endif
164100010601     C                   endif
164200010601     C*
164300010601     C                   if        vinflg = '2'
164400010601     C                   eval      flgStato = '2'
164500010601     C                   endif
164600010601     C*
164700010601     C                   ENDSR
164800010601     C***
164900010601
165000010601
165100010601
165200010330
165300040119      /TITLE Invio dei dati al punto operativo.
165400040119     C     invio         BEGSR
165500040119     C*
165600040119     C* 1° invio FIVAT
165700040119     C                   reset                   dscmz
165800040119     C                   move      vlrpoi        cmzdst
165900040119     C                   eval      cmzfld = 'FIVATWWR'
166000040119     C                   eval      cmzmbd = vlrhdl
166100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166200040119     C***                if        prmfir = *blanks
166300040119     C                   eval      cmzfla = 'FIVAT00F'
166400040119     C                   eval      cmzmba = 'FIVAT00F'
166500040119     C***                else
166600040119     C***                eval      cmzfla = prmfir
166700040119     C***                eval      cmzmba = prmfir
166800040119     C***                endif
166900040119     C                   eval      cmznrr = *zeros
167000040119     C                   move      §ctrokvt      cmznrr
167100040119     C                   eval      cmzlba = vlrfl1
167200040119     C                   call(e)   'TIS711C'
167300040119     C                   parm                    dscmz
167400040119     C                   parm      *blanks       esito
167500040119     C                   if        %error
167600040119     C                             or cmzerr = '1'
167700040119     C                             or esito  = '1'
167800040119     C                   eval      wrkesito = '3'
167900040119     C                   else
168000040119     C*
168100040119     C* 2° invio FIVAB
168200040119     C                   reset                   dscmz
168300040119     C                   move      vlrpoi        cmzdst
168400040119     C                   eval      cmzfld = vlrfou
168500040119     C                   eval      cmzmbd = vlrhdl
168600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168700040119     C***                if        prmfir = *blanks
168800040119     C                   eval      cmzfla = 'FIVAB00F'
168900040119     C                   eval      cmzmba = 'FIVAB00F'
169000040119     C***                else
169100040119     C***                eval      cmzfla = prmfir
169200040119     C***                eval      cmzmba = prmfir
169300040119     C***                endif
169400040119     C                   eval      cmznrr = *zeros
169500040119     C                   move      §ctrokvb      cmznrr
169600040119     C                   eval      cmzlba = vlrfl1
169700040119     C                   call(e)   'TIS711C'
169800040119     C                   parm                    dscmz
169900040119     C                   parm      *blanks       esito
170000040119     C                   if        %error
170100040119     C                             or cmzerr = '1'
170200040119     C                             or esito  = '1'
170300040119     C                   eval      wrkesito = '3'
170400040119     C                   endif
170500040119     C                   endif
170600040119     C*
170700040119     C                   ENDSR
170800040119     C***
170900010601
171000010601
171100010601
171200010601
171300010601      /TITLE Invio dei dati al punto operativo.
171400010601     C     opeini        BEGSR
171500010601     C*
171600010601     C* Inizializzo flag e contatori operativi
171700010601     C                   movel     '0'           flgGiro           1
171800010601     C                   movel     '0'           flgMulti          1
171900010601     C                   movel     '1'           flgStato          1
172000010615     C                   movel     '0'           flgOk             1
172100010601     C                   z-add     *zeros        cntNonEl         10 0
172200010601     C                   z-add     *zeros        depfil            3 0
172300010601     C                   z-add     *zeros        invfil            3 0
172400010601     C*
172500010601     C                   ENDSR
172600010601     C***
172700070326
172800070326
172900070326
173000070326
173100080916     C     *pssr         BEGSR
173200070329     C*
173300080916     C                   if        %open(tivin00r)
173400080916     C                   close     tivin00r
173500080916     C                   endif
173600080916     C                   if        %open(fivabwwr)
173700080916     C                   close     fivabwwr
173800080916     C                   endif
173900080916     C                   if        %open(fivatwwr)
174000080916     C                   close     fivatwwr
174100080916     C                   endif
174200070326     C*
174300070326     C* Effettuo la chiamata al CLLE preposto
174400080916     C                   call(e)   'TITVVTC'
174500080916     C                   parm                    parccm
174600080916     C                   parm                    parmbr
174700080916     C                   parm      '2'           paropz
174800070326     C*
174900080916     C                   eval      wrkesito = '2'
175000070404     C*
175100080916     C                   seton                                        LR
175200070326     C*
175300080916     C                   ENDSR     '*CANCL'
175400070326     C***
175500070326
175600070326
175700010330
175800010330
175900000613     C     *inzsr        BEGSR
176000990910     C*
176100990910     C     *entry        plist
176200990920     C                   parm                    tivlrds
176300990921     C                   parm      wrkesito      esito
176400000724     C                   parm                    prmlit
176500000710     C                   parm                    prmfir
176600010330     C*
176700010330     C* CALCOLA LA DATA CORRENTE
176800010330     C                   time                    wn14             14 0
176900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
177000010330     C                   z-add     wn8           g08dat
177100010330     C                   z-add     *zeros        g08inv
177200010330     C                   movel     '0'           g08err
177300010330     C                   call      'XSRDA8'
177400010330     C                   parm                    wlbda8
177500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
177600000613     C*
177700000613     C                   ENDSR
177800000613     C***
177900990908
178000090921     Otitv1r8p  E            riepilogo         2
178100990915     O                                              'Upload via Internet'
178200990915     O                                           +1 'Traduzione TIVIN00R -'
178300021025     O                                           +1 'FIVABWWR'
178400990915     O                                           +1 'Testate bolle'
178500990915     O          E            riepilogo   2
178600990915     O                       wrkdata
178700990915     O                       wrkora              +1
178800990915     O                       procname            +1
178900990915     O          E            riepilogo   2
179000990915     O                                              'Cliente..................:'
179100010605     O                       VABCCM        z     +1
179200990915     O          E            riepilogo   2
179300990920     O                                              'Riferimento Strategi.....:'
179400990920     O                       vlrhdl              +1
179500990915     O          E            riepilogo   2
179600990915     O                                              'Giusti...................:'
179700040119     O                       §CTROKVB      2   +  1
179800990915     O          E            riepilogo   2
179900990915     O                                              'Sbagliati e corretti.....:'
180000971022     O                       §CTRMO        2   +  1
180100990915     O          E            riepilogo   2
180200990915     O                                              'Sbagliati e scartati.....:'
180300971022     O                       §CTRNO        2   +  1
180400000613
180500090921     Otitv1r8ps E            testdett          2
180600000613     O                                              'Upload via Internet'
180700000613     O                                           +1 'Traduzione TIVIN00R -'
180800021025     O                                           +1 'FIVABWWR'
180900000613     O                                           +1 'Testate bolle'
181000000616     O          E            testdett    3
181100000613     O                                           +2 'N° rec'
181200000613     O                                           +3 'Anteprima contenuto'
181300000616     O          E            rigadett    2
181400000613     O                       rrnum               +2
181500000621     O                       recko               +3
181600000616     O          E            findett     2
181700000613     O                       wrkdata
181800000613     O                       wrkora              +1
181900000613     O                       procname            +1
182000000616     O          E            findett     2
182100000613     O                                              'Cliente..................:'
182200010605     O                       VABCCM        z     +1
182300000616     O          E            findett     2
182400000613     O                                              'Riferimento Strategi.....:'
182500000613     O                       vlrhdl              +1
182600000616     O          E            findett     2
182700000613     O                                              'Giusti...................:'
182800040119     O                       §CTROKVB      2   +  1
182900000616     O          E            findett     2
183000000613     O                                              'Sbagliati e corretti.....:'
183100000613     O                       §CTRMO        2   +  1
183200000616     O          E            findett     2
183300000613     O                                              'Sbagliati e scartati.....:'
183400000613     O                       §CTRNO        2   +  1
183500000613** CMD - COMANDI CL
183600090921OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1R8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
183700090921OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1R8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
183800090921DLTOVR FILE(TITV1R8P TITV1R8PS) LVL(*)
183900010606
184000000613
184100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
184200090921VABRMN;VABRMA;FIL001;W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNAS;
184300100112FIL002;W_VATNOTA1;W_VATNOTB1;VATNOT_E;
184400090921
