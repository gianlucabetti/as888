000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080527     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800100318     Ftitv1w1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100100318     Ftitv1w1ps O    f  198        PRINTER usropn
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
002200080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040128     D tisi95ds      e ds
003100080527     D fivabds       e ds                  extname(fivab00f)
003200080527     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003300080527     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003400010330     D esito           s              1
003500010330     D prmlit          s             10
003600010330     D prmfir          s             10
003700010330     D wrkesito        s                   like(esito)
003800010330     D wrkdata         s               d
003900010330     D wrkora          s               t
004000010330     D rrnum           s              6  0 INZ(*zeros)
004100010330     D recko           s            150    INZ(*blanks)
004200011113     D depcmd          s            150    INZ(*blanks)
004300040510     D parccm          s              8    INZ(*blanks)
004400040119     D parmbr          s             10    INZ(*blanks)
004500040119     D paropz          s              1    INZ(*blanks)
004600040119     D chkcall         s              1    INZ(*blanks)
004700080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004800080418     D wFlgCAS         s              1    INZ(*blanks)
004900030715     D*------------------
005000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005100030715     D*------------------
005200030715     D SkSplitFLD      S             10    DIM(100)
005300030715     D SkSplitCSV      S            256    DIM(100)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700100318     D CharSOS         S              1
005800030715     D posDa           S              3  0 INZ(*zeros)
005900030715     D posA            S              3  0 INZ(*zeros)
006000030715     D i               s              3  0 INZ(1)
006100030715     D wGiro           s              1  0 INZ(*zeros)
006200070823     D wVATNOT_A       s                   LIKE(VATNOT)
006300060331     D wVATNOT_B       s                   LIKE(VATNOT)
006400060331     D wVATNOT_E       s                   LIKE(VATNOT)
006500030822     D*------------------
006600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006700030822     D*------------------
006800030822     D posDaDft        S              3  0 INZ(*zeros)
006900030822     D posADft         S              3  0 INZ(*zeros)
007000030822     D j               s              3  0 INZ(1)
007100030822     D wGiroDft        s              1  0 INZ(*zeros)
007200080418     D*------------------
007300080418     D* Costanti
007400080418     D*------------------
007500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007700100318
007800100318     D*------------------
007900100318     D* LINKING A DEFINIZIONI ESTERNE
008000100318     D*------------------
008100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008300100318
008400010330
008500010330
008600010330
008700990915     C                   time                    wrkdata
008800990915     C                   time                    wrkora
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200010601     C*
009300010601     C                   exsr      opeini
009400010605     C                   exsr      rwvab
009500010601     C*
009600010601     C                   seton                                        lr
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010200010601     C*--------------------------------------------------------
010300010601     C     PREELA        BEGSR
010400010601     C*
010500010601     C* SE OCCORRE SPEDIRE IN FILIALE
010600010601     C                   if        invfil <> *zeros and
010700010601     C                             flgGiro = '0'
010800010601     C*
010900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011000010601     C                   eval      flgGiro = '1'
011100000724     C*
011200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011400010601     C     invfil        chain     azorg01l
011500000724     C                   if        %found
011600000724     C                   movel(p)  CMD(1)        depcmd
011700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011800000724     C*
011900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000724     C                   PARM                    depcmd
012300000724     C                   PARM                    LENGH
012400000724     C*
012500000724     C                   endif
012600000616     C*
012700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012800000616     C                   movel(p)  CMD(2)        depcmd
012900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013000000616     C*
013100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013400000616     C                   PARM                    depcmd
013500000616     C                   PARM                    LENGH
013600000613     C*
013700100318     C                   if        not %open(titv1w1ps)
013800100318     C                   open      titv1w1ps
013900070329     C                   except    testdett
014000000613     C                   endif
014100010601     C*
014200010601     C                   endif
014300010601     C*
014400010601     C                   ENDSR
014500010601     C***
014600010601
014700010601
014800010601
014900010601
015000010601     C*--------------------------------------------------------
015100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015200010601     C*--------------------------------------------------------
015300010601     C     ENDELA        BEGSR
015400990915     C*
015500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015600040119     C*
015700040119     C* Effettuo la chiamata al CLLE preposto
015800040119     C                   call(e)   'TITVVTC'
015900040510     C                   parm                    parccm
016000040119     C                   parm                    parmbr
016100040119     C                   parm      '2'           paropz
016200050201     C*
016300050201     C* Effettuo lancio TISI95 solo x chiusura
016400050201     C                   CLEAR                   TISI95DS
016500050201     C                   EVAL      I95TLA = 'C'
016600050201     C                   CALL      'TISI95R'
016700050201     C                   PARM                    TISI95DS
016800000613     C*
016900100318     C                   if        %open(titv1w1ps)
017000000616     C                   except    findett
017100100318     C                   close     titv1w1ps
017200000613     C                   endif
017300000616     C*
017400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017500011113     C                   Z-ADD     150           LENGH            15 5
017600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017700000616     C                   PARM                    CMD(3)
017800000616     C                   PARM                    LENGH
017900000616     C*
018000010601     C                   ENDSR
018100010601     C***
018200010601
018300010601
018400010601
018500000613
018600000613     C*--------------------------------------------------------
018700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018800000613     C*--------------------------------------------------------
018900000621     C     STPR          BEGSR
019000000613     C*
019100100318     C                   if        not %open(titv1w1p)
019200100318     C   50              open      titv1w1p
019300990915     C                   endif
019400990915     C*
019500030715     C   50              except    riepilogo
019600990915     C*
019700100318     C                   if        %open(titv1w1p)
019800100318     C   50              close     titv1w1p
019900990914     C                   endif
020000000613     C*
020100000613     C                   ENDSR
020200000613     C***
020300000613
020400010601
020500010601
020600010330     C*--------------------------------------------------------
020700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020800010330     C*--------------------------------------------------------
020900010605     C     RWVAB         BEGSR
021000010330     C*
021100010330     C                   if        not %open(tivin00r)
021200010330     C                   open      tivin00r
021300010330     C                   endif
021400021025     C                   if        not %open(fivabwwr)
021500021025     C                   open      fivabwwr
021600010330     C                   endif
021700040119     C*
021800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021900040119     C                   exsr      prevat
022000040119     C*
022100040119     C                   if        chkcall = '0'
022200040119     C*
022300040119     C                   if        not %open(fivatwwr)
022400040119     C                   open      fivatwwr
022500040119     C                   endif
022600010330     C*
022700040119     C                   clear                   §CTROKVB          7 0
022800040119     C                   clear                   §CTROKVT          7 0
022900040119     C                   clear                   §CTRMO            7 0
023000040119     C                   clear                   §CTRNO            7 0
023100070823     C*
023200070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
023300070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
023400100318     C***                exsr      inzvar
023500100318     C***                exsr      impvab
023600010330     C*
023700030822     C                   do        *HIVAL
023800010330     C*
023900030822     C                   read      tivin00r                               70
024000010618     C*
024100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024200010618     C                   if        vinflg = '1'
024300010618     C                   eval      flgOk = '1'
024400010618     C                   endif
024500040510     C*
024600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024700040510     C                   z-add     *zeros        wLenVINDTA        4 0
024800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
025000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
025100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
025200040510     C                   z-add     1             wFlgVINDTA
025300040510     C                   leave
025400040510     C                   endif
025500040510     C                   enddo
025600010618     C*
025700040510     C                   if        vindta > *blanks AND
025800040510     C                             wFlgVINDTA = 1
025900010330     C                   add       1             rrnum
026000010330     C*
026100010601     C                   if        *in70 = *off and
026200010330     C                             (vinflg = *blanks
026300010330     C                              or vinflg = '0'
026400010330     C                              or vinflg = '2')
026500010330     C*
026600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026700010711     C                   if        vinflg = *blanks or vinflg = '0'
026800010711     C                   clear                   vinmsg
026900010711     C                   endif
027000010601     C*
027100070925     C* Aggiungo un byte a blanks all'inizio d ogni record
027200070925     C                   eval      vindta  = ' ' + %trim(vindta)
027300070925     C*
027400080527     C                   exsr      inzvar
027500100318     C                   exsr      normCSV
027600010605     C                   exsr      impvab
027700010601     C*
027800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027900010605     C                   eval      depfil = VABLNP
028000010601     C                   exsr      repfil
028100010601     C                   if        depfil = invfil
028200021025     C                   if        vlrpoi = 999
028300030822     C                   move(P)   invfil        VABFGS
028400021025     C                   else
028500030822     C                   move(P)   vlrpoi        VABFGS
028600021025     C                   endif
028700010601     C*
028800010601     C                   exsr      PREELA
028900010601     C*
029000010604     C* Ebbene...
029100010604     C*
029200040119     C  N31              add       1             §CTROKVB
029300040119     C   32              add       1             §CTRMO
029400040119     C   31              add       1             §CTRNO
029500080418     C*
029600030822     C                   if        wGiro = 2
029700080513     C* Scrivo sempre il VAT
029800080513     C  N31              exsr      wrivat
029900080527     C                   exsr      chkvab
030000080513     C                   endif
030100010604     C*
030200010604     C                   if        *in31 = *off and
030300010604     C                             *in32 = *off
030400010604     C                   eval      vinflg = '1'
030500010604     C                   else
030600010604     C                   eval      recko = vindta
030700010604     C                   except    rigadett
030800010604     C                   eval      vinflg = '2'
030900010604     C                   endif
031000010604     C                   endif
031100010601     C*
031200010604     C                   endif
031300010604     C*
031400010330     C                   else
031500010330     C                   eval      vinflg = '1'
031600010330     C                   endif
031700010601     C*
031800010601     C  N70              update    tivin000
031900010330     C*
032000030822     C  N70              enddo
032100040119     C*
032200080527     C* Verifico dati rimasti "in cannna"...
032300080527     C                   seton                                        65
032400080527     C                   exsr      chkvab
032500080527     C*
032600040119     C                   endif                                                  (endif - chkcall)
032700010601     C*
032800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032900010601     C                   if        cntNonEl = *zeros or
033000010601     C                             flgMulti = '0'
033100010330     C* Se non ci sono record con errori ...
033200010601     C                   if        §ctrno = 0 and
033300010604     C                             §ctrmo = 0 and
033400010601     C                             flgStato <> '2'
033500010330     C* ... restituisco esito OK.
033600010330     C                   eval      wrkesito = '0'
033700010330     C                   else
033800040119     C                   if        §ctrokvb > 0
033900010330     C                   eval      wrkesito = '1'
034000010330     C                   else
034100010615     C                   if        flgOk = '0'
034200010615     C                   eval      wrkesito = '2'
034300010615     C                   else
034400010615     C                   eval      wrkesito = '6'
034500010615     C                   endif
034600010330     C                   endif
034700010330     C                   endif
034800010601     C                   else
034900010601     C                   eval      wrkesito = '9'
035000010601     C                   endif
035100010330     C*
035200010330     C                   if        %open(tivin00r)
035300010330     C                   close     tivin00r
035400010330     C                   endif
035500021025     C                   if        %open(fivabwwr)
035600021025     C                   close     fivabwwr
035700010330     C                   endif
035800040512     C                   if        %open(fivatwwr)
035900040512     C                   close     fivatwwr
036000040512     C                   endif
036100010601     C*
036200010601     C                   if        vlrpoi <> 999
036300010601     C                   eval      invfil = vlrpoi
036400010601     C                   endif
036500010330     C*
036600040119     C                   if        §ctrokvb > 0
036700010601     C                             and invfil > *zeros
036800010330     C                   exsr      invio
036900010330     C                   endif
037000010601     C*
037100010618     C                   if        flgGiro = '1'
037200010601     C                   exsr      endela
037300010618     C                   endif
037400010330     C*
037500010330     C                   ENDSR
037600010330     C***
037700080527
037800080527
037900080527
038000080527     C*----------------------------------------------------*
038100080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
038200080527     C*----------------------------------------------------*
038300080527     C     CHKVAB        BEGSR
038400080527     C*
038500080527     C* Scrivo il VAB solo a rottura d spedizione
038600080527     C                   if        depspe = *zeros
038700080527     C  N31              eval      fivabds_sav = fivabds
038800080527     C                   eval      depspe = vabNSP
038900080527     C                   else
039000080527     C                   if        depspe = vabNSP AND *in65 = *off
039100080527     C                   add       VABNCL        sav_VABNCL
039200080527     C                   add       VABPKB        sav_VABPKB
039300080527     C                   else
039400080527     C  N31              eval      fivabds_ok = fivabds_sav
039500080527     C  N31              exsr      wrivab
039600080527     C  N31              eval      fivabds_sav = fivabds
039700080527     C                   eval      depspe = vabNSP
039800080527     C                   endif
039900080527     C                   endif
040000080527     C*
040100080527     C                   ENDSR
040200040119
040300040119
040400040119
040500040119     C*----------------------------------------------------*
040600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040700040119     C*----------------------------------------------------*
040800040510     C     WRIVAB        BEGSR
040900080513     C*
041000080513     C* Gestisco l'eventuale rottura x numero spedizione
041100080527     C                   if        VABNSP <> depspe or *in65 = *on
041200070301     C                   if        wVATNOT_E <> *blanks
041300070301     C                   eval      VABCTM = '7Q'
041400070301     C                   endif
041500040119     C                   write     fivab000                                     => scarico il VAB
041600080513     C                   endif
041700040119     C*
041800040119     C                   ENDSR
041900040119
042000040119
042100040119
042200040119     C*----------------------------------------------------*
042300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042400040119     C*----------------------------------------------------*
042500040510     C     WRIVAT        BEGSR
042600060512     C*
042700060512     C* Inizializzo i campi chiave
042800060512     C                   eval      VATFGS = VABFGS
042900060512     C                   eval      VATAAS = VABAAS
043000060512     C                   eval      VATCCM = VABCCM
043100070823     C                   eval      VATNRS = VABNRS
043200060512     C                   eval      VATNSP = VABNSP
043300060512     C                   eval      VATLNP = VABLNP
043400040714     C*
043500040714     C* Solo se VATNOT valorizzato scarico il buffer
043600040714     C                   if        VATNOT <> *blanks
043700040119     C*
043800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
043900040510     C                   if        VATTRC = *blanks
044000040510     C                   eval      VATTRC = 'E'
044100040510     C                   endif
044200040510     C                   write     FIVAT000
044300040119     C                   add       1             §CTROKVT
044400040714     C*
044500040714     C                   endif
044600060331     C*
044700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
044800060331     C                   if        wVATNOT_A <> *blanks
044900060331     C                   eval      VATNOT = wVATNOT_A
045000060331     C                   eval      VATTRC = 'A'
045100060331     C                   write     FIVAT000
045200060331     C                   add       1             §CTROKVT
045300060331     C                   endif
045400060331     C*
045500060331     C                   if        wVATNOT_B <> *blanks
045600060331     C                   eval      VATNOT = wVATNOT_B
045700060331     C                   eval      VATTRC = 'B'
045800060331     C                   write     FIVAT000
045900060331     C                   add       1             §CTROKVT
046000060331     C                   endif
046100060331     C*
046200060331     C                   if        wVATNOT_E <> *blanks
046300060331     C                   eval      VATNOT = wVATNOT_E
046400060331     C                   eval      VATTRC = 'E'
046500060331     C                   write     FIVAT000
046600060331     C                   add       1             §CTROKVT
046700060331     C                   endif
046800040119     C*
046900040119     C                   ENDSR
047000040119
047100040119
047200040119
047300040119     C*----------------------------------------------------*
047400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047500040119     C*----------------------------------------------------*
047600040119     C     PREVAT        BEGSR
047700040119     C*
047800040119     C* Compongo il nome del membro da dare al FIVATWWR
047900040119     C                   eval      parmbr = vlrhdl
048000040119     C                   movel     'M'           parmbr
048100040510     C                   eval      parccm = vlrksc
048200040119     C                   eval      paropz = '1'
048300040119     C* Effettuo la chiamata al CLLE preposto
048400040119     C                   call(e)   'TITVVTC'
048500040119     C                   parm                    parccm
048600040119     C                   parm                    parmbr
048700040119     C                   parm                    paropz
048800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048900040119     C                   if        %error
049000040119     C                   movel     '1'           chkcall
049100040119     C                   else
049200040119     C                   movel     '0'           chkcall
049300040119     C                   endif
049400040119     C*
049500040119     C                   ENDSR
049600010601
049700010601
049800010601
049900010330     C*----------------------------------------------------*
050000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050100010330     C*----------------------------------------------------*
050200040119     C     INZVAR        BEGSR
050300080527     C*
050400080527     C                   setoff                                       65
050500010330     C*
050600030822     C* Inizializzo variabili di wrk
050700020204     C                   Z-ADD     *zeros        Num5_0            5 0
050800060331     C                   MOVEL     *blanks       wVATNOT_A
050900060331     C                   MOVEL     *blanks       wVATNOT_B
051000060331     C                   MOVEL     *blanks       wVATNOT_E
051100030822     C*
051200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051300030822     C                   CLEAR                   SkSplitCSV
051400030822     C                   EVAL      i = 1
051500030822     C                   EVAL      posDa = *zeros
051600030822     C                   EVAL      posA  = *zeros
051700080527     C*
051800080527     C                   CLEAR                   FIVABDS
051900080527     C                   CLEAR                   FIVAT000
052000030822     C*
052100030822     C* Reimposto i valori di default
052200030822     C                   EXSR      DEFCAM
052300030822     C*
052400010330     C                   ENDSR
052500010330     C*----------------------------------------------------*
052600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052700010330     C*----------------------------------------------------*
052800010330     C     DEFCAM        BEGSR
052900030822     C*
053000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053100030715     C* e delimitatore testo.
053200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053500100318     C*
053600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053700100318     C                   EVAL      CharSOS = CharNUM
053800030822     C*
053900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
054100030715     C                   SETON                                        50
054200030715     C                   ELSE
054300030715     C                   SETOFF                                       50
054400030715     C                   ENDIF
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
082700040113     C*
082800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083100080429     C                             %trim(intcol(5))<>*blanks
083200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083400080429     C                                      %trim(intcol(5))
083500070823     C     ';':CharCSV   XLATE     vindta        vindta
083600040510     C                   ENDIF
083700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083800031201     C     minu:maiu     XLATE     vindta        vindta
083900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084000030715     C                   DOW       posDa <= %len(%trim(vindta))
084100030715     C*
084200030715     C* Gestisco il 1° campo
084300030715     C                   IF        i = 1
084400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084500030715     C                   EVAL      posDa = 1
084600030715     C* Eseguo lo scan x trovare la fine del primo campo
084700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C* X i campi successivi al 1°
085200030715     C                   ELSE
085300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085400030715     C                   EVAL      posDa = posA + 1
085500030715     C* Eseguo lo scan x trovare la fine del campo corrente
085600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085700030715     C* Gestisco l'ultimo campo
085800030715     C                   IF        posA = *zeros
085900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086000030715     C* A questo punto "estraggo" il campo corrente
086100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086200030715     C                                                    (posA-posDa))
086300030715     C                   LEAVE
086400030715     C                   ELSE
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C                   ENDIF
086900030715     C                   ENDIF
087000030715     C* Incremento il contatore d campo
087100030715     C                   EVAL      i = i +1
087200030715     C                   ENDDO
087300030715     C*
087400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087500030715     C* delimitazione testo
087600030715     C                   EVAL      i = 1
087700030715     C                   DOW       i < %elem(SkSplitFLD)
087800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088000030715     C                   EVAL      i = i + 1
088100030715     C                   ENDDO
088200030715     C                   EVAL      wGiro = 1
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
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiNum=*on
117300030721     C                   Z-ADD     PiVal         VABCAS
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABCAS = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABCAS'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** CCM
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on      AND
118600030721     C                             PiVal<=9999999 AND
118700030721     C                             PiVal>*zeros
118800030721     C                   Z-ADD     PiVal         VABCCM
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      VABCCM = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300040510     C                             + ' ' + 'VABCCM'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** LNP
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on    AND
120100030721     C                             PiVal<=999   AND
120200030721     C                             PiVal>*zeros
120300030721     C                   Z-ADD     PiVal         VABLNP
120400030721     C                   ELSE
120500040715     C                   SETON                                        32
120600030721     C                   EVAL      VABLNP = *zeros
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800040510     C                             + ' ' + 'VABLNP'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** NRS
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on AND
121600030721     C                             PiVal<=99
121700030721     C                   Z-ADD     PiVal         VABNRS
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABNRS'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** LNA
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on    AND
122900030721     C                             PiVal<=999   AND
123000030721     C                             PiVal>*zeros
123100030721     C                   Z-ADD     PiVal         VABLNA
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      VABLNA = *zeros
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABLNA'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** CTR
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on  AND
124400030721     C                             PiVal<=999
124500030721     C                   Z-ADD     PiVal         VABCTR
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABCTR'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** IAS
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABIAS
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABIAS'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** VLB
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABVLB
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABVLB'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** QFT
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiNum=*on
128100030721     C                   Z-ADD     PiVal         VABQFT
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABQFT'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** NCD
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on      AND
129300030721     C                             PiVal<=9999999
129400030721     C                   Z-ADD     PiVal         VABNCD
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABNCD'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** NCA
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on      AND
130600030721     C                             PiVal<=9999999
130700030721     C                   Z-ADD     PiVal         VABNCA
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABNCA'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** ZNC
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on AND
131900030721     C                             PiVal<=99
132000030721     C                   Z-ADD     PiVal         VABZNC
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABZNC'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** DCR
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on       AND
133200030721     C                             PiVal<=99999999
133300030721     C                   Z-ADD     PiVal         VABDCR
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABDCR'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** HCR
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on   AND
134500030721     C                             PiVal<=9999
134600030721     C                   Z-ADD     PiVal         VABHCR
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABHCR'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** VMD
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiNum=*on
135800030721     C                   Z-ADD     PiVal         VABVMD
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABVMD'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** ANT
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on        AND
137000030721     C                             PiVal<=999999999
137100030721     C                   Z-ADD     PiVal         VABANT
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABANT'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** CMO
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on
138300030721     C                   Z-ADD     PiVal         Num5_0
138400030721     C                   MOVEL(p)  Num5_0        VABCMO
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      VABCMO = *zeros
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABCMO'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200040510     C*
139300040510     C* Reperisco i campi del FIVAT
139400040510     C*** VATNOT
139500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139700040510     C                   ENDIF
139800040510     C*** VATTRC
139900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140100040510     C                   ENDIF
140200060331     C*** VATNOT_A
140300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140500060331     C                   ENDIF
140600060331     C*** VATNOT_B
140700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140900060331     C                   ENDIF
141000060331     C*** VATNOT_E
141100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141300060331     C                   ENDIF
141400070823     C*
141500071029     C***  ===>  Gestione campi "particolari" dentro ciclo
141600100318     C*** NSP/RMN/RMA
141700100318     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
141800080513     C                   EVAL      VABRMA=SkSplitCSV(i)
141900080513     C                   EVAL      PiStr=SkSplitCSV(i)
142000080513     C                   EXSR      CHKNUM
142100080513     C                   IF        PiInt=*on
142200080513     C                   Z-ADD     PiVal         VABNSP
142300080513     C                   Z-ADD     PiVal         VATNSP
142400100318     C                   Z-ADD     PiVal         VABRMN
142500080513     C                   ELSE
142600080513     C                   SETON                                        31
142700080513     C                   EVAL      VABNSP = *zeros
142800080513     C                   EVAL      VATNSP = *zeros
142900100318     C                   EVAL      VABRMN = *zeros
143000080513     C                   EVAL      vinmsg = %trimr(vinmsg)
143100100318     C                             + ' ' + 'VABNSP VATNSP VABRMN'
143200080513     C                   ENDIF
143300080513     C                   ENDIF
143400080513     C*** NCL
143500080521     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNCL'
143600080513     C                   EVAL      PiStr=SkSplitCSV(i)
143700080513     C                   EXSR      CHKNUM
143800080513     C                   IF        PiInt=*on
143900080527     C                   Z-ADD     PiVal         VABNCL
144000080513     C                   ELSE
144100080513     C                   SETON                                        32
144200080527     C                   EVAL      VABNCL = *zeros
144300080513     C                   EVAL      vinmsg = %trimr(vinmsg)
144400080513     C                             + ' ' + 'VABNCL'
144500080513     C                   ENDIF
144600080513     C                   ENDIF
144700080513     C*** PKB
144800080521     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
144900080513     C                   EVAL      PiStr=SkSplitCSV(i)
145000080513     C                   EXSR      CHKNUM
145100080513     C                   IF        PiNum=*on
145200080527     C                   Z-ADD     PiVal         VABPKB
145300080513     C                   ELSE
145400080513     C                   SETON                                        32
145500080527     C                   EVAL      VABPKB = *zeros
145600080513     C                   EVAL      vinmsg = %trimr(vinmsg)
145700080513     C                             + ' ' + 'VABPKB'
145800080513     C                   ENDIF
145900080513     C                   ENDIF
146000070925     C*
146100071029     C***  <===  -----------------------------------------
146200040510     C*
146300030715     C                   ENDIF
146400030715     C                   EVAL      i = i + 1
146500030715     C                   ENDDO
146600070925     C*
146700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146800070925     C*
146900071029     C***  <===  -----------------------------------------
147000070925     C*
147100070925     C* Considerazioni finali su CBO/CAS
147200100318     C                   IF        wFlgCAS = '1'    OR
147300100318     C                             VABCAS  > *zeros
147400070925     C                   IF        VABCBO = '1'
147500070925     C                   EVAL      VABCBO = '4'
147600070925     C                   ENDIF
147700070925     C                   IF        VABCBO = '2'
147800070925     C                   EVAL      VABCBO = '6'
147900070925     C                   ENDIF
148000070925     C                   ENDIF
148100100326     C*
148200100326     C                   IF        VABCAS > *zeros AND
148300100326     C                             VABVCA = *blanks
148400100326     C                   EVAL      VABVCA = 'EUR'
148500100326     C                   ENDIF
148600100326     C*
148700100326     C                   IF        VABIAS > *zeros AND
148800100326     C                             VABVAS = *blanks
148900100326     C                   EVAL      VABVAS = 'EUR'
149000100326     C                   ENDIF
149100070925     C***  <===  ----------------------------
149200040510     C*
149300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149400040510     C                   IF        VABNSP = *zeros
149500040510     C                   SETON                                        31
149600040510     C                   ENDIF
149700040715     C*
149800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149900040715     C                   IF        VABLNP = *zeros
150000040715     C                   SETON                                        31
150100040715     C                   ENDIF
150200040510     C*
150300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150400040510     C                   IF        VABRMN = *zeros
150500040510     C                   Z-ADD     VABNSP        VABRMN
150600040510     C                   ENDIF
150700040510     C*
150800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150900040510     C                   IF        VABRMA = *blanks
151000040714     C                   MOVEL     VABRMN        VABRMA
151100040510     C                   ENDIF
151200040128     C*
151300040510     C* Se provincia nn valorizzata la reperisco
151400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151500040510     C                   IF        VABPRD = *blanks
151600040128     C                   CLEAR                   TISI95DS
151700040128     C                   EVAL      I95TCN = '3'
151800040128     C                   Z-ADD     datcor        I95DAT
151900040128     C                   EVAL      I95NAR = VABNZD
152000040128     C                   EVAL      I95CAP = VABCAD
152100040128     C                   EVAL      I95LOC = VABLOD
152200040128     C                   CALL      'TISI95R'
152300040128     C                   PARM                    TISI95DS
152400040128     C                   EVAL      VABPRD = O95PRV
152500040510     C                   ENDIF
152600020204     C*
152700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152800020204     C                   EXSR      CHKIMPDIV
152900030715     C                   ENDIF
153000020204     C*
153100010607     C                   ENDSR
153200010607     C*----------------------------------------------------*
153300010601
153400020204
153500020204     C*----------------------------------------------------*
153600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153700020204     C*----------------------------------------------------*
153800020204     C     CHKIMPDIV     BEGSR
153900020204     C*
154000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154100020204     C                   Z-ADD     *zeros        wrkDec            9 9
154200020204     C*
154300020204     C* Come prima cosa effettuo considerazioni sulla divisa
154400020204     C                   IF        vabIAS > *zeros
154500020204     C                   IF        vabVAS <> 'EUR'
154600020204     C                   EVAL      vabVAS =  'ITL'
154700020204     C                   ENDIF
154800020204     C                   ENDIF
154900020204     C*
155000020204     C                   IF        vabCAS > *zeros
155100020204     C                   IF        vabVCA <> 'EUR'
155200020204     C                   EVAL      vabVCA =  'ITL'
155300020204     C                   ENDIF
155400020204     C                   ENDIF
155500020204     C*
155600020204     C                   IF        vabVMD > *zeros
155700020204     C                   IF        vabVAD <> 'EUR'
155800020204     C                   EVAL      vabVAD =  'ITL'
155900020204     C                   ENDIF
156000020204     C                   ENDIF
156100020204     C*
156200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156300020204     C                   Z-ADD     vabIAS        wrkDec
156400020204     C                   IF        wrkDec > *zeros
156500020204     C                   IF        vabVAS = 'ITL'
156600020204     C                   EVAL      vabIAS = *zeros
156700020204     C                   ENDIF
156800020204     C                   ENDIF
156900020204     C*
157000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157100020204     C                   Z-ADD     vabCAS        wrkDec
157200020204     C                   IF        wrkDec > *zeros
157300020204     C                   IF        vabVCA = 'ITL'
157400020204     C                   EVAL      vabCAS = *zeros
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157900020204     C                   Z-ADD     vabVMD        wrkDec
158000020204     C                   IF        wrkDec > *zeros
158100020204     C                   IF        vabVAD = 'ITL'
158200020204     C                   EVAL      vabVMD = *zeros
158300020204     C                   ENDIF
158400020204     C                   ENDIF
158500020204     C*
158600020204     C                   ENDSR
158700020204     C***
158800010330
158900100318
159000100318
159100100318     C*----------------------------------------------------*
159200100318     C*  CONTROLLO NUMERICITA' CAMPI
159300100318     C*----------------------------------------------------*
159400100318     C     CHKNUM        BEGSR
159500100318     C*
159600100318     C                   IF        PiDecChr = *blanks
159700100318     C                   EVAL      PiDecChr = CharNUM
159800100318     C                   ENDIF
159900100318     C*
160000100318     C                   callp     UBISNUM_Check(PiStr
160100100318     C                                          :PiDecChr
160200100318     C                                          :PiVal
160300100318     C                                          :PiNum
160400100318     C                                          :PiInt)
160500100318     C*
160600100318     C                   ENDSR
160700100318     C***
160800010601
160900010601
161000010601
161100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161200010601     C     repfil        BEGSR
161300010601     C*
161400010601     C                   if        invfil = *zeros and
161500010601     C                             depfil > *zeros and
161600010629     C                             (vinflg = *blanks or
161700010629     C                              vinflg = *zeros)
161800010601     C
161900010601     C                   eval      invfil = depfil
162000010601     C                   endif
162100010601     C*
162200010601     C                   if        depfil <> invfil and
162300010601     C                             invfil > *zeros
162400010601     C                   eval      flgMulti = '1'
162500010601     C                   if        vinflg = *blanks
162600010601     C                   add       1             cntNonEl
162700010601     C                   endif
162800010601     C                   endif
162900010601     C*
163000010601     C                   if        vinflg = '2'
163100010601     C                   eval      flgStato = '2'
163200010601     C                   endif
163300010601     C*
163400010601     C                   ENDSR
163500010601     C***
163600010601
163700010601
163800010601
163900010330
164000040119      /TITLE Invio dei dati al punto operativo.
164100040119     C     invio         BEGSR
164200040119     C*
164300040119     C* 1° invio FIVAT
164400040119     C                   reset                   dscmz
164500040119     C                   move      vlrpoi        cmzdst
164600040119     C                   eval      cmzfld = 'FIVATWWR'
164700040119     C                   eval      cmzmbd = vlrhdl
164800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164900040119     C***                if        prmfir = *blanks
165000040119     C                   eval      cmzfla = 'FIVAT00F'
165100040119     C                   eval      cmzmba = 'FIVAT00F'
165200040119     C***                else
165300040119     C***                eval      cmzfla = prmfir
165400040119     C***                eval      cmzmba = prmfir
165500040119     C***                endif
165600040119     C                   eval      cmznrr = *zeros
165700040119     C                   move      §ctrokvt      cmznrr
165800040119     C                   eval      cmzlba = vlrfl1
165900040119     C                   call(e)   'TIS711C'
166000040119     C                   parm                    dscmz
166100040119     C                   parm      *blanks       esito
166200040119     C                   if        %error
166300040119     C                             or cmzerr = '1'
166400040119     C                             or esito  = '1'
166500040119     C                   eval      wrkesito = '3'
166600040119     C                   else
166700040119     C*
166800040119     C* 2° invio FIVAB
166900040119     C                   reset                   dscmz
167000040119     C                   move      vlrpoi        cmzdst
167100040119     C                   eval      cmzfld = vlrfou
167200040119     C                   eval      cmzmbd = vlrhdl
167300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167400040119     C***                if        prmfir = *blanks
167500040119     C                   eval      cmzfla = 'FIVAB00F'
167600040119     C                   eval      cmzmba = 'FIVAB00F'
167700040119     C***                else
167800040119     C***                eval      cmzfla = prmfir
167900040119     C***                eval      cmzmba = prmfir
168000040119     C***                endif
168100040119     C                   eval      cmznrr = *zeros
168200040119     C                   move      §ctrokvb      cmznrr
168300040119     C                   eval      cmzlba = vlrfl1
168400040119     C                   call(e)   'TIS711C'
168500040119     C                   parm                    dscmz
168600040119     C                   parm      *blanks       esito
168700040119     C                   if        %error
168800040119     C                             or cmzerr = '1'
168900040119     C                             or esito  = '1'
169000040119     C                   eval      wrkesito = '3'
169100040119     C                   endif
169200040119     C                   endif
169300040119     C*
169400040119     C                   ENDSR
169500040119     C***
169600010601
169700010601
169800010601
169900010601
170000010601      /TITLE Invio dei dati al punto operativo.
170100010601     C     opeini        BEGSR
170200010601     C*
170300010601     C* Inizializzo flag e contatori operativi
170400010601     C                   movel     '0'           flgGiro           1
170500010601     C                   movel     '0'           flgMulti          1
170600010601     C                   movel     '1'           flgStato          1
170700010615     C                   movel     '0'           flgOk             1
170800010601     C                   z-add     *zeros        cntNonEl         10 0
170900010601     C                   z-add     *zeros        depfil            3 0
171000010601     C                   z-add     *zeros        invfil            3 0
171100010601     C*
171200010601     C                   ENDSR
171300010601     C***
171400070326
171500070326
171600070326
171700070326
171800070326     C     *pssr         BEGSR
171900070329     C*
172000070329     C                   if        %open(tivin00r)
172100070329     C                   close     tivin00r
172200070329     C                   endif
172300070329     C                   if        %open(fivabwwr)
172400070329     C                   close     fivabwwr
172500070329     C                   endif
172600070329     C                   if        %open(fivatwwr)
172700070329     C                   close     fivatwwr
172800070329     C                   endif
172900070326     C*
173000070326     C* Effettuo la chiamata al CLLE preposto
173100070326     C                   call(e)   'TITVVTC'
173200070326     C                   parm                    parccm
173300070326     C                   parm                    parmbr
173400070326     C                   parm      '2'           paropz
173500070326     C*
173600070326     C                   eval      wrkesito = '2'
173700070404     C*
173800070404     C                   seton                                        LR
173900070326     C*
174000070326     C                   ENDSR     '*CANCL'
174100070326     C***
174200100318
174300100318
174400100318
174500100318     C     normCSV       BEGSR
174600100318     C*
174700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
174800100318     C* separatori campi
174900100318     C                   if        CharTXT <> *blanks
175000100318     C                   z-add     *zeros        wLenVINDTA        4 0
175100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175200100318     C                   setoff                                       80
175300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175600100318     C                   if        not *in80
175700100318     C                   seton                                        80
175800100318     C                   else
175900100318     C                   setoff                                       80
176000100318     C                   endif
176100100318     C                   endif
176200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176300100318     C                             and *in80 = *on
176400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176500100318     C                   endif
176600100318     C                   enddo
176700100318     C                   endif
176800100318     C*
176900100318     C                   ENDSR
177000100318     C***
177100070326
177200010330
177300010330
177400000613     C     *inzsr        BEGSR
177500990910     C*
177600990910     C     *entry        plist
177700990920     C                   parm                    tivlrds
177800990921     C                   parm      wrkesito      esito
177900000724     C                   parm                    prmlit
178000000710     C                   parm                    prmfir
178100010330     C*
178200010330     C* CALCOLA LA DATA CORRENTE
178300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
178400100318     C                   eval      datcor = %dec(%date() : *ISO)
178500000613     C*
178600000613     C                   ENDSR
178700000613     C***
178800990908
178900100318     Otitv1w1p  E            riepilogo         2
179000990915     O                                              'Upload via Internet'
179100990915     O                                           +1 'Traduzione TIVIN00R -'
179200021025     O                                           +1 'FIVABWWR'
179300990915     O                                           +1 'Testate bolle'
179400990915     O          E            riepilogo   2
179500990915     O                       wrkdata
179600990915     O                       wrkora              +1
179700990915     O                       procname            +1
179800990915     O          E            riepilogo   2
179900990915     O                                              'Cliente..................:'
180000010605     O                       VABCCM        z     +1
180100990915     O          E            riepilogo   2
180200990920     O                                              'Riferimento Strategi.....:'
180300990920     O                       vlrhdl              +1
180400990915     O          E            riepilogo   2
180500990915     O                                              'Giusti...................:'
180600040119     O                       §CTROKVB      2   +  1
180700990915     O          E            riepilogo   2
180800990915     O                                              'Sbagliati e corretti.....:'
180900971022     O                       §CTRMO        2   +  1
181000990915     O          E            riepilogo   2
181100990915     O                                              'Sbagliati e scartati.....:'
181200971022     O                       §CTRNO        2   +  1
181300000613
181400100318     Otitv1w1ps E            testdett          2
181500000613     O                                              'Upload via Internet'
181600000613     O                                           +1 'Traduzione TIVIN00R -'
181700021025     O                                           +1 'FIVABWWR'
181800000613     O                                           +1 'Testate bolle'
181900000616     O          E            testdett    3
182000000613     O                                           +2 'N° rec'
182100000613     O                                           +3 'Anteprima contenuto'
182200000616     O          E            rigadett    2
182300000613     O                       rrnum               +2
182400000621     O                       recko               +3
182500000616     O          E            findett     2
182600000613     O                       wrkdata
182700000613     O                       wrkora              +1
182800000613     O                       procname            +1
182900000616     O          E            findett     2
183000000613     O                                              'Cliente..................:'
183100010605     O                       VABCCM        z     +1
183200000616     O          E            findett     2
183300000613     O                                              'Riferimento Strategi.....:'
183400000613     O                       vlrhdl              +1
183500000616     O          E            findett     2
183600000613     O                                              'Giusti...................:'
183700040119     O                       §CTROKVB      2   +  1
183800000616     O          E            findett     2
183900000613     O                                              'Sbagliati e corretti.....:'
184000000613     O                       §CTRMO        2   +  1
184100000616     O          E            findett     2
184200000613     O                                              'Sbagliati e scartati.....:'
184300000613     O                       §CTRNO        2   +  1
184400000613** CMD - COMANDI CL
184500100318OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1W1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
184600100318OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1W1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
184700100318DLTOVR FILE(TITV1W1P TITV1W1PS) LVL(*)
184800010606
184900000613
185000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
185100100318X001;W_VABNSP;W_VABNCL;W_VABPKB;VATNOT_E;VABCBO;VABRSD;VABIND;VABLOD;
185200100318VABCAD;VABPRD;VABCAS;VABFFD;VABIAS;VABNZD;VATNOT_B;VABNOT
185300100318
185400100318
185500100318
