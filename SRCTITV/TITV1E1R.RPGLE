000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800080225     Ftitv1e1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100080225     Ftitv1e1ps O    f  198        PRINTER usropn
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
004000080219     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040510     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000030715     D CharCSV         S              1
005100030715     D CharTXT         S              1
005200030715     D CharNUM         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700070823     D wVATNOT_A       s                   LIKE(VATNOT)
005800060331     D wVATNOT_B       s                   LIKE(VATNOT)
005900060331     D wVATNOT_E       s                   LIKE(VATNOT)
006000030822     D*------------------
006100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006200030822     D*------------------
006300030822     D posDaDft        S              3  0 INZ(*zeros)
006400030822     D posADft         S              3  0 INZ(*zeros)
006500030822     D j               s              3  0 INZ(1)
006600030822     D wGiroDft        s              1  0 INZ(*zeros)
006700010330     D*------------------
006800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006900010330     D*------------------
007000010330     D WLBDA8          DS                  INZ
007100010330     D  G08DAT                 1      8  0
007200010330     D  G08INV                 9     16  0
007300010330     D  G08ERR                17     17
007400010330     D  G08TGI                18     22  0
007500031201     D*------------------
007600031201     D* Costanti
007700031201     D*------------------
007800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008000010330
008100010330
008200010330
008300990915     C                   time                    wrkdata
008400990915     C                   time                    wrkora
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800010601     C*
008900010601     C                   exsr      opeini
009000010605     C                   exsr      rwvab
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601     C*--------------------------------------------------------
009700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009800010601     C*--------------------------------------------------------
009900010601     C     PREELA        BEGSR
010000010601     C*
010100010601     C* SE OCCORRE SPEDIRE IN FILIALE
010200010601     C                   if        invfil <> *zeros and
010300010601     C                             flgGiro = '0'
010400010601     C*
010500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010600010601     C                   eval      flgGiro = '1'
010700000724     C*
010800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011000010601     C     invfil        chain     azorg01l
011100000724     C                   if        %found
011200000724     C                   movel(p)  CMD(1)        depcmd
011300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011400000724     C*
011500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011600011113     C                   Z-ADD     150           LENGH            15 5
011700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000724     C                   PARM                    depcmd
011900000724     C                   PARM                    LENGH
012000000724     C*
012100000724     C                   endif
012200000616     C*
012300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012400000616     C                   movel(p)  CMD(2)        depcmd
012500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012600000616     C*
012700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012800011113     C                   Z-ADD     150           LENGH            15 5
012900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013000000616     C                   PARM                    depcmd
013100000616     C                   PARM                    LENGH
013200000613     C*
013300080225     C                   if        not %open(titv1e1ps)
013400080225     C                   open      titv1e1ps
013500070329     C                   except    testdett
013600000613     C                   endif
013700010601     C*
013800010601     C                   endif
013900010601     C*
014000010601     C                   ENDSR
014100010601     C***
014200010601
014300010601
014400010601
014500010601
014600010601     C*--------------------------------------------------------
014700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014800010601     C*--------------------------------------------------------
014900010601     C     ENDELA        BEGSR
015000990915     C*
015100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015200040119     C*
015300040119     C* Effettuo la chiamata al CLLE preposto
015400040119     C                   call(e)   'TITVVTC'
015500040510     C                   parm                    parccm
015600040119     C                   parm                    parmbr
015700040119     C                   parm      '2'           paropz
015800050201     C*
015900050201     C* Effettuo lancio TISI95 solo x chiusura
016000050201     C                   CLEAR                   TISI95DS
016100050201     C                   EVAL      I95TLA = 'C'
016200050201     C                   CALL      'TISI95R'
016300050201     C                   PARM                    TISI95DS
016400000613     C*
016500080225     C                   if        %open(titv1e1ps)
016600000616     C                   except    findett
016700080225     C                   close     titv1e1ps
016800000613     C                   endif
016900000616     C*
017000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017100011113     C                   Z-ADD     150           LENGH            15 5
017200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017300000616     C                   PARM                    CMD(3)
017400000616     C                   PARM                    LENGH
017500000616     C*
017600010601     C                   ENDSR
017700010601     C***
017800010601
017900010601
018000010601
018100000613
018200000613     C*--------------------------------------------------------
018300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018400000613     C*--------------------------------------------------------
018500000621     C     STPR          BEGSR
018600000613     C*
018700080225     C                   if        not %open(titv1e1p)
018800080225     C   50              open      titv1e1p
018900990915     C                   endif
019000990915     C*
019100030715     C   50              except    riepilogo
019200990915     C*
019300080225     C                   if        %open(titv1e1p)
019400080225     C   50              close     titv1e1p
019500990914     C                   endif
019600000613     C*
019700000613     C                   ENDSR
019800000613     C***
019900000613
020000010601
020100010601
020200010330     C*--------------------------------------------------------
020300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020400010330     C*--------------------------------------------------------
020500010605     C     RWVAB         BEGSR
020600010330     C*
020700010330     C                   if        not %open(tivin00r)
020800010330     C                   open      tivin00r
020900010330     C                   endif
021000021025     C                   if        not %open(fivabwwr)
021100021025     C                   open      fivabwwr
021200010330     C                   endif
021300040119     C*
021400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021500040119     C                   exsr      prevat
021600040119     C*
021700040119     C                   if        chkcall = '0'
021800040119     C*
021900040119     C                   if        not %open(fivatwwr)
022000040119     C                   open      fivatwwr
022100040119     C                   endif
022200010330     C*
022300040119     C                   clear                   §CTROKVB          7 0
022400040119     C                   clear                   §CTROKVT          7 0
022500040119     C                   clear                   §CTRMO            7 0
022600040119     C                   clear                   §CTRNO            7 0
022700070823     C*
022800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
022900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
023000080225     C***                exsr      inzvar
023100080225     C***                exsr      impvab
023200010330     C*
023300030822     C                   do        *HIVAL
023400010330     C*
023500030822     C                   read      tivin00r                               70
023600010618     C*
023700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023800010618     C                   if        vinflg = '1'
023900010618     C                   eval      flgOk = '1'
024000010618     C                   endif
024100040510     C*
024200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024300040510     C                   z-add     *zeros        wLenVINDTA        4 0
024400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024800040510     C                   z-add     1             wFlgVINDTA
024900040510     C                   leave
025000040510     C                   endif
025100040510     C                   enddo
025200010618     C*
025300040510     C                   if        vindta > *blanks AND
025400040510     C                             wFlgVINDTA = 1
025500010330     C                   add       1             rrnum
025600010330     C*
025700010601     C                   if        *in70 = *off and
025800010330     C                             (vinflg = *blanks
025900010330     C                              or vinflg = '0'
026000010330     C                              or vinflg = '2')
026100010330     C*
026200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026300010711     C                   if        vinflg = *blanks or vinflg = '0'
026400010711     C                   clear                   vinmsg
026500010711     C                   endif
026600010601     C*
026700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
026800070925     C                   eval      vindta  = ' ' + %trim(vindta)
026900070925     C*
027000010330     C                   exsr      inzvar
027100010605     C                   exsr      impvab
027200010601     C*
027300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027400010605     C                   eval      depfil = VABLNP
027500010601     C                   exsr      repfil
027600010601     C                   if        depfil = invfil
027700021025     C                   if        vlrpoi = 999
027800030822     C                   move(P)   invfil        VABFGS
027900021025     C                   else
028000030822     C                   move(P)   vlrpoi        VABFGS
028100021025     C                   endif
028200010601     C*
028300010601     C                   exsr      PREELA
028400010601     C*
028500010604     C* Ebbene...
028600010604     C*
028700040119     C  N31              add       1             §CTROKVB
028800040119     C   32              add       1             §CTRMO
028900040119     C   31              add       1             §CTRNO
029000030822     C                   if        wGiro = 2
029100040510     C* Scrivo sempre il VAT
029200040510     C  N31              exsr      wrivat
029300040510     C* Scrivo il VAB solo a rottura d spedizione
029400040119     C                   if        depspe =  *zeros OR
029500040119     C                             depspe <> vabNSP
029600040119     C  N31              exsr      wrivab
029700040119     C  N31              eval      depspe = vabNSP
029800040119     C                   endif
029900040119     C                   endif
030000010604     C*
030100010604     C                   if        *in31 = *off and
030200010604     C                             *in32 = *off
030300010604     C                   eval      vinflg = '1'
030400010604     C                   else
030500010604     C                   eval      recko = vindta
030600010604     C                   except    rigadett
030700010604     C                   eval      vinflg = '2'
030800010604     C                   endif
030900010604     C                   endif
031000010601     C*
031100010604     C                   endif
031200010604     C*
031300010330     C                   else
031400010330     C                   eval      vinflg = '1'
031500010330     C                   endif
031600010601     C*
031700010601     C  N70              update    tivin000
031800010330     C*
031900030822     C  N70              enddo
032000040119     C*
032100040119     C                   endif                                                  (endif - chkcall)
032200010601     C*
032300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032400010601     C                   if        cntNonEl = *zeros or
032500010601     C                             flgMulti = '0'
032600010330     C* Se non ci sono record con errori ...
032700010601     C                   if        §ctrno = 0 and
032800010604     C                             §ctrmo = 0 and
032900010601     C                             flgStato <> '2'
033000010330     C* ... restituisco esito OK.
033100010330     C                   eval      wrkesito = '0'
033200010330     C                   else
033300040119     C                   if        §ctrokvb > 0
033400010330     C                   eval      wrkesito = '1'
033500010330     C                   else
033600010615     C                   if        flgOk = '0'
033700010615     C                   eval      wrkesito = '2'
033800010615     C                   else
033900010615     C                   eval      wrkesito = '6'
034000010615     C                   endif
034100010330     C                   endif
034200010330     C                   endif
034300010601     C                   else
034400010601     C                   eval      wrkesito = '9'
034500010601     C                   endif
034600010330     C*
034700010330     C                   if        %open(tivin00r)
034800010330     C                   close     tivin00r
034900010330     C                   endif
035000021025     C                   if        %open(fivabwwr)
035100021025     C                   close     fivabwwr
035200010330     C                   endif
035300040512     C                   if        %open(fivatwwr)
035400040512     C                   close     fivatwwr
035500040512     C                   endif
035600010601     C*
035700010601     C                   if        vlrpoi <> 999
035800010601     C                   eval      invfil = vlrpoi
035900010601     C                   endif
036000010330     C*
036100040119     C                   if        §ctrokvb > 0
036200010601     C                             and invfil > *zeros
036300010330     C                   exsr      invio
036400010330     C                   endif
036500010601     C*
036600010618     C                   if        flgGiro = '1'
036700010601     C                   exsr      endela
036800010618     C                   endif
036900010330     C*
037000010330     C                   ENDSR
037100010330     C***
037200040119
037300040119
037400040119
037500040119     C*----------------------------------------------------*
037600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037700040119     C*----------------------------------------------------*
037800040510     C     WRIVAB        BEGSR
037900040119     C*
038000060331     C* Gestisco l'eventuale rottura x numero spedizione
038100070823     C                   if        VABNSP <> depspe
038200070301     C                   if        wVATNOT_E <> *blanks
038300070301     C                   eval      VABCTM = '7Q'
038400070301     C                   endif
038500040119     C                   write     fivab000                                     => scarico il VAB
038600060331     C                   endif
038700040119     C*
038800040119     C                   ENDSR
038900040119
039000040119
039100040119
039200040119     C*----------------------------------------------------*
039300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039400040119     C*----------------------------------------------------*
039500040510     C     WRIVAT        BEGSR
039600060512     C*
039700060512     C* Inizializzo i campi chiave
039800060512     C                   eval      VATFGS = VABFGS
039900060512     C                   eval      VATAAS = VABAAS
040000060512     C                   eval      VATCCM = VABCCM
040100070823     C                   eval      VATNRS = VABNRS
040200060512     C                   eval      VATNSP = VABNSP
040300060512     C                   eval      VATLNP = VABLNP
040400040714     C*
040500040714     C* Solo se VATNOT valorizzato scarico il buffer
040600040714     C                   if        VATNOT <> *blanks
040700040119     C*
040800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040900040510     C                   if        VATTRC = *blanks
041000040510     C                   eval      VATTRC = 'E'
041100040510     C                   endif
041200040510     C                   write     FIVAT000
041300040119     C                   add       1             §CTROKVT
041400040714     C*
041500040714     C                   endif
041600060331     C*
041700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041800060331     C                   if        wVATNOT_A <> *blanks
041900060331     C                   eval      VATNOT = wVATNOT_A
042000060331     C                   eval      VATTRC = 'A'
042100060331     C                   write     FIVAT000
042200060331     C                   add       1             §CTROKVT
042300060331     C                   endif
042400060331     C*
042500060331     C                   if        wVATNOT_B <> *blanks
042600060331     C                   eval      VATNOT = wVATNOT_B
042700060331     C                   eval      VATTRC = 'B'
042800060331     C                   write     FIVAT000
042900060331     C                   add       1             §CTROKVT
043000060331     C                   endif
043100060331     C*
043200060331     C                   if        wVATNOT_E <> *blanks
043300060331     C                   eval      VATNOT = wVATNOT_E
043400060331     C                   eval      VATTRC = 'E'
043500060331     C                   write     FIVAT000
043600060331     C                   add       1             §CTROKVT
043700060331     C                   endif
043800040119     C*
043900040119     C                   ENDSR
044000040119
044100040119
044200040119
044300040119     C*----------------------------------------------------*
044400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044500040119     C*----------------------------------------------------*
044600040119     C     PREVAT        BEGSR
044700040119     C*
044800040119     C* Compongo il nome del membro da dare al FIVATWWR
044900040119     C                   eval      parmbr = vlrhdl
045000040119     C                   movel     'M'           parmbr
045100040510     C                   eval      parccm = vlrksc
045200040119     C                   eval      paropz = '1'
045300040119     C* Effettuo la chiamata al CLLE preposto
045400040119     C                   call(e)   'TITVVTC'
045500040119     C                   parm                    parccm
045600040119     C                   parm                    parmbr
045700040119     C                   parm                    paropz
045800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045900040119     C                   if        %error
046000040119     C                   movel     '1'           chkcall
046100040119     C                   else
046200040119     C                   movel     '0'           chkcall
046300040119     C                   endif
046400040119     C*
046500040119     C                   ENDSR
046600010601
046700010601
046800010601
046900010330     C*----------------------------------------------------*
047000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047100010330     C*----------------------------------------------------*
047200040119     C     INZVAR        BEGSR
047300010330     C*
047400030822     C* Inizializzo variabili di wrk
047500020204     C                   Z-ADD     *zeros        Num5_0            5 0
047600060331     C                   MOVEL     *blanks       wVATNOT_A
047700060331     C                   MOVEL     *blanks       wVATNOT_B
047800060331     C                   MOVEL     *blanks       wVATNOT_E
047900030822     C*
048000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048100030822     C                   CLEAR                   FIVAB000
048200040119     C                   CLEAR                   FIVAT000
048300030822     C                   CLEAR                   SkSplitCSV
048400030822     C                   EVAL      i = 1
048500030822     C                   EVAL      posDa = *zeros
048600030822     C                   EVAL      posA  = *zeros
048700030822     C*
048800030822     C* Reimposto i valori di default
048900030822     C                   EXSR      DEFCAM
049000030822     C*
049100010330     C                   ENDSR
049200010330     C*----------------------------------------------------*
049300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049400010330     C*----------------------------------------------------*
049500010330     C     DEFCAM        BEGSR
049600030822     C*
049700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049800030715     C* e delimitatore testo.
049900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050200030822     C*
050300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
050500030715     C                   SETON                                        50
050600030715     C                   ELSE
050700030715     C                   SETOFF                                       50
050800030715     C                   ENDIF
050900030822     C*
051000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051100030822     C                   EVAL      posDaDft = 1
051200030822     C                   EVAL      posADft  = 0
051300030822     C                   EVAL      wGiroDft = 0
051400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051500030822     C                             posDaDft > 0
051600030822     C*
051700030822     C* Gestisco il 1° giro
051800030822     C                   IF        wGiroDft = 0
051900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052100030822     C* Incremento il contatore dei "giri"
052200030822     C                   EVAL      wGiroDft = 1
052300030822     C                   ELSE
052400030822     C                   EVAL      posDaDft = posADft
052500030822     C                   ENDIF
052600030822     C* Eseguo lo scan x trovare la fine del campo corrente
052700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052800030822     C*
052900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053100030822     C                   IF        posDaDft > 0 AND
053200030822     C                             posADft  > 0
053300030822     C* NCL
053400030822     C                   IF        %subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):1:3)
053700030822     C                             = 'NCL'
053800030822     C                   EVAL      PiStr=%trim(%subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100030822     C                   EXSR      CHKNUM
054200030822     C                   IF        PiInt=*on
054300030822     C                   Z-ADD     PiVal         VABNCL
054400030822     C                   ENDIF
054500030822     C                   ENDIF
054600030822     C* CCM
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'CCM'
055100030822     C                   EVAL      PiStr=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   EXSR      CHKNUM
055500030822     C                   IF        PiInt=*on
055600030822     C                   Z-ADD     PiVal         VABCCM
055700030822     C                   ENDIF
055800030822     C                   ENDIF
055900030822     C* LNP
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'LNP'
056400030822     C                   EVAL      PiStr=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   EXSR      CHKNUM
056800030822     C                   IF        PiInt=*on
056900030822     C                   Z-ADD     PiVal         VABLNP
057000030822     C                   ENDIF
057100030822     C                   ENDIF
057200030822     C* NRS
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'NRS'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiInt=*on
058200030822     C                   Z-ADD     PiVal         VABNRS
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* CTR
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'CTR'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABCTR
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* PKB
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'PKB'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiNum=*on
060800030822     C                   Z-ADD     PiVal         VABPKB
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* VLB
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'VLB'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiNum=*on
062100030822     C                   Z-ADD     PiVal         VABVLB
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* QFT
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'QFT'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiNum=*on
063400030822     C                   Z-ADD     PiVal         VABQFT
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* CBO
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'CBO'
064200030822     C                   EVAL      VABCBO=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* TSP
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'TSP'
065100030822     C                   EVAL      VABTSP=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* VAS
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'VAS'
066000030822     C                   EVAL      VABVAS=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* VCA
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'VCA'
066900030822     C                   EVAL      VABVCA=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300070925     C* TIC
067400070925     C                   IF        %subst(
067500070925     C                             %subst(vlrppt:posDaDft+1:
067600070925     C                             posADft-posDaDft-1):1:3)
067700070925     C                             = 'TIC'
067800070925     C                   EVAL      VABTIC=%trim(%subst(
067900070925     C                             %subst(vlrppt:posDaDft+1:
068000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100070925     C                   ENDIF
068200030822     C* GCA
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'GCA'
068700030822     C                   EVAL      VABGCA=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* CTM
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'CTM'
069600030822     C                   EVAL      VABCTM=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* FFD
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'FFD'
070500030822     C                   EVAL      VABFFD=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* VAD
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'VAD'
071400030822     C                   EVAL      VABVAD=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* GMA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'GMA'
072300030822     C                   EVAL      VABGMA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* GGA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'GGA'
073200030822     C                   EVAL      VABGGA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* GVA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GVA'
074100030822     C                   EVAL      VABGVA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* TC1
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'TC1'
075000030822     C                   EVAL      VABTC1=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* TC2
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'TC2'
075900030822     C                   EVAL      VABTC2=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300040714     C* VATTRC
076400040714     C                   IF        %subst(
076500040714     C                             %subst(vlrppt:posDaDft+1:
076600040714     C                             posADft-posDaDft-1):1:3)
076700040714     C                             = 'TRC'
076800040714     C                   EVAL      VATTRC=%trim(%subst(
076900040714     C                             %subst(vlrppt:posDaDft+1:
077000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100040714     C                   ENDIF
077200030822     C* ...
077300030822     C                   ENDIF
077400030822     C                   ENDDO
077500020204     C*
077600010330     C                   ENDSR
077700010607     C*----------------------------------------------------*
077800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
077900010607     C*----------------------------------------------------*
078000010607     C     IMPVAB        BEGSR
078100010607     C*
078200010607     C                   SETOFF                                       3132
078300030515     C*
078400010607     C                   MOVEL     datcor        VABAAS
078500010607     C                   MOVE      datcor        VABMGS
078600010607     C*
078700030715     C**********
078800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078900030715     C**********
079000030715     C                   IF        wGiro = *zeros
079100040113     C*
079200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079400040510     C                             %trim(intcol(3))<>*blanks
079500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079600040113     C                                      %trim(intcol(3))
079700070823     C     ';':CharCSV   XLATE     vindta        vindta
079800040510     C                   ENDIF
079900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080000031201     C     minu:maiu     XLATE     vindta        vindta
080100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080200030715     C                   DOW       posDa <= %len(%trim(vindta))
080300030715     C*
080400030715     C* Gestisco il 1° campo
080500030715     C                   IF        i = 1
080600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080700030715     C                   EVAL      posDa = 1
080800030715     C* Eseguo lo scan x trovare la fine del primo campo
080900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C* X i campi successivi al 1°
081400030715     C                   ELSE
081500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081600030715     C                   EVAL      posDa = posA + 1
081700030715     C* Eseguo lo scan x trovare la fine del campo corrente
081800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081900030715     C* Gestisco l'ultimo campo
082000030715     C                   IF        posA = *zeros
082100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082200030715     C* A questo punto "estraggo" il campo corrente
082300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082400030715     C                                                    (posA-posDa))
082500030715     C                   LEAVE
082600030715     C                   ELSE
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C                   ENDIF
083100030715     C                   ENDIF
083200030715     C* Incremento il contatore d campo
083300030715     C                   EVAL      i = i +1
083400030715     C                   ENDDO
083500030715     C*
083600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083700030715     C* delimitazione testo
083800030715     C                   EVAL      i = 1
083900030715     C                   DOW       i < %elem(SkSplitFLD)
084000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084200030715     C                   EVAL      i = i + 1
084300030715     C                   ENDDO
084400030715     C                   EVAL      wGiro = 1
084500030715     C                   ELSE
084600030715     C                   EVAL      wGiro = 2
084700031204     C**********
084800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
084900031204     C* d separatore campo
085000031204     C**********
085100031204     C                   EVAL      vindta = %trim(vindta)
085200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085300031204     C                   EVAL      vindta = %subst(vindta:2)
085400031204     C                   ENDDO
085500031204     C*
085600031204     C                   Z-ADD     *zeros        lunghInput        4 0
085700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
085900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086100031204     C                   ENDDO
086200030715     C**********
086300030715     C* Effettuo lo split del campo dati d input
086400030715     C**********
086500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086600030715     C                   DOW       posDa <= %len(%trim(vindta))
086700030715     C*
086800030715     C* Gestisco il 1° campo
086900030715     C                   IF        i = 1
087000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087100030715     C                   EVAL      posDa = 1
087200030715     C* Eseguo lo scan x trovare la fine del primo campo
087300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087400030715     C* A questo punto "estraggo" il campo corrente
087500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087600030715     C                                                    (posA-posDa))
087700030715     C* X i campi successivi al 1°
087800030715     C                   ELSE
087900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088000030715     C                   EVAL      posDa = posA + 1
088100031204     C* Verifico che nn vi sia il campo nn valorizzato
088200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088300031204     C* Se campo nn valorizzato skippo al prossimo
088400031204     C                   EVAL      SkSplitCSV(i) = *blanks
088500031204     C                   EVAL      posA  = posA + 1
088600031204     C                   ELSE
088700030715     C* Eseguo lo scan x trovare la fine del campo corrente
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* Gestisco l'ultimo campo
089000030715     C                   IF        posA = *zeros
089100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089200030715     C* A questo punto "estraggo" il campo corrente
089300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089400030715     C                                                    (posA-posDa))
089500030715     C                   LEAVE
089600030715     C                   ELSE
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C                   ENDIF
090100031204     C                   ENDIF
090200030715     C                   ENDIF
090300030715     C* Incremento il contatore d campo
090400030715     C                   EVAL      i = i +1
090500030715     C                   ENDDO
090600030715     C*
090700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090800030715     C* delimitazione testo
090900030715     C                   EVAL      i = 1
091000030715     C                   DOW       i < %elem(SkSplitCSV)
091100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091300030715     C                   EVAL      i = i + 1
091400030715     C                   ENDDO
091500030715     C*
091600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
091700030715     C                   EVAL      i = 1
091800030715     C                   DOW       i < %elem(SkSplitFLD)
091900030715     C*
092000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092200030715     C                             SkSplitCSV(i) <> *blanks
092300030721     C*** ATB
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092500030721     C                   EVAL      VABATB = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030715     C*** RSD
092800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
092900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093200030715     C* ==
093300030715     C                   ENDIF
093400030721     C*** RD2
093500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
093600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
093700030721     C                   ENDIF
093800030715     C*** IND
093900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094000030715     C                   EVAL      VABIND = SkSplitCSV(i)
094100030715     C                   ENDIF
094200030715     C*** LOD
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
094400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
094500030715     C                   ENDIF
094600030715     C*** PRD
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
094800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030721     C*** NZD
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
095200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030715     C*** RMA
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
095600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** NOT
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** NT2
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
096400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** VCA
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
096800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** VAS
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
097200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** FFD
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
097600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030721     C*** GC1
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** GC2
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** TSP
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
098800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098900030721     C                   ENDIF
099000040510     C*** CBO
099100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099300040510     C                   ENDIF
099400030721     C*** NAS
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
099600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** TIC
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** GCA
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** XCO
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
100800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** CTM
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** TCR
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
101600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** CTS
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** FTM
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** VAD
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
102800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** GMA
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** GGA
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
103600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** GVA
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** TC1
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** TC2
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
104800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** SCL
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** RMO
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** NMO
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*
106300030721     C* Reperisco quindi i campi numerici...
106400040510     C*** NSP
106500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
106600040510     C                   EVAL      PiStr=SkSplitCSV(i)
106700040510     C                   EXSR      CHKNUM
106800040510     C                   IF        PiInt=*on
106900040510     C                   Z-ADD     PiVal         VABNSP
107000040510     C                   ELSE
107100040510     C                   SETON                                        31
107200040510     C                   EVAL      VABNSP = *zeros
107300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107400040510     C                             + ' ' + 'VABNSP'
107500040510     C                   ENDIF
107600040510     C                   ENDIF
107700071106     C*** RMN
107800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
107900071106     C                   EVAL      PiStr=SkSplitCSV(i)
108000071106     C                   EXSR      CHKNUM
108100071106     C                   IF        PiInt=*on
108200071106     C                   Z-ADD     PiVal         VABRMN
108300071106     C                   ELSE
108400071106     C                   SETON                                        32
108500071106     C                   EVAL      VABRMN = *zeros
108600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
108700071106     C                             + ' ' + 'VABRMN'
108800071106     C                   ENDIF
108900071106     C                   ENDIF
109000030721     C*** CAD
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109200030721     C                   EVAL      PiStr=SkSplitCSV(i)
109300030721     C                   EXSR      CHKNUM
109400030721     C                   IF        PiInt=*on
109500030721     C                   Z-ADD     PiVal         Num5_0
109600030721     C                   MOVEL(p)  Num5_0        VABCAD
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      VABCAD = *zeros
110000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110100030721     C                             + ' ' + 'VABCAD'
110200030721     C                   ENDIF
110300030721     C                   ENDIF
110400030721     C*** NCL
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
110600030721     C                   EVAL      PiStr=SkSplitCSV(i)
110700030721     C                   EXSR      CHKNUM
110800030721     C                   IF        PiInt=*on
110900030721     C                   Z-ADD     PiVal         VABNCL
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      VABNCL = *zeros
111300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111400040122     C                             + ' ' + 'VABNCL'
111500030721     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** PKB
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiNum=*on
112200030721     C                   Z-ADD     PiVal         VABPKB
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      VABPKB = *zeros
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700030721     C                             + ' ' + 'VABPKB'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** CAS
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiNum=*on
113500030721     C                   Z-ADD     PiVal         VABCAS
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      VABCAS = *zeros
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000030721     C                             + ' ' + 'VABCAS'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** CCM
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiInt=*on      AND
114800030721     C                             PiVal<=9999999 AND
114900030721     C                             PiVal>*zeros
115000030721     C                   Z-ADD     PiVal         VABCCM
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABCCM = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500040510     C                             + ' ' + 'VABCCM'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** LNP
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiInt=*on    AND
116300030721     C                             PiVal<=999   AND
116400030721     C                             PiVal>*zeros
116500030721     C                   Z-ADD     PiVal         VABLNP
116600030721     C                   ELSE
116700040715     C                   SETON                                        32
116800030721     C                   EVAL      VABLNP = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000040510     C                             + ' ' + 'VABLNP'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** NRS
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on AND
117800030721     C                             PiVal<=99
117900030721     C                   Z-ADD     PiVal         VABNRS
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABNRS'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** LNA
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on    AND
119100030721     C                             PiVal<=999   AND
119200030721     C                             PiVal>*zeros
119300030721     C                   Z-ADD     PiVal         VABLNA
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      VABLNA = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABLNA'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** CTR
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on  AND
120600030721     C                             PiVal<=999
120700030721     C                   Z-ADD     PiVal         VABCTR
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABCTR'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** IAS
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiNum=*on
121900030721     C                   Z-ADD     PiVal         VABIAS
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABIAS'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** VLB
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiNum=*on
123100030721     C                   Z-ADD     PiVal         VABVLB
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABVLB'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** QFT
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABQFT
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABQFT'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** NCD
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on      AND
125500030721     C                             PiVal<=9999999
125600030721     C                   Z-ADD     PiVal         VABNCD
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABNCD'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** NCA
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on      AND
126800030721     C                             PiVal<=9999999
126900030721     C                   Z-ADD     PiVal         VABNCA
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABNCA'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** ZNC
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on AND
128100030721     C                             PiVal<=99
128200030721     C                   Z-ADD     PiVal         VABZNC
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABZNC'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** DCR
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on       AND
129400030721     C                             PiVal<=99999999
129500030721     C                   Z-ADD     PiVal         VABDCR
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABDCR'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** HCR
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on   AND
130700030721     C                             PiVal<=9999
130800030721     C                   Z-ADD     PiVal         VABHCR
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABHCR'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** VMD
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiNum=*on
132000030721     C                   Z-ADD     PiVal         VABVMD
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABVMD'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** ANT
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on        AND
133200030721     C                             PiVal<=999999999
133300030721     C                   Z-ADD     PiVal         VABANT
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABANT'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** CMO
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on
134500030721     C                   Z-ADD     PiVal         Num5_0
134600030721     C                   MOVEL(p)  Num5_0        VABCMO
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      VABCMO = *zeros
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABCMO'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400040510     C*
135500040510     C* Reperisco i campi del FIVAT
135600040510     C*** VATNOT
135700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
135800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
135900040510     C                   ENDIF
136000040510     C*** VATTRC
136100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
136200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
136300040510     C                   ENDIF
136400060331     C*** VATNOT_A
136500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
136600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
136700060331     C                   ENDIF
136800060331     C*** VATNOT_B
136900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
137000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
137100071106     C                   EVAL      VABNOT    = %trim(SkSplitCSV(i))
137200060331     C                   ENDIF
137300060331     C*** VATNOT_E
137400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
137500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
137600060331     C                   ENDIF
137700070823     C*
137800071029     C***  ===>  Gestione campi "particolari" dentro ciclo
137900071204     C*
138000080225     C*** CBO
138100080225     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
138200080225     C                   IF        SkSplitCSV(i) = 'F'
138300080225     C                   EVAL      VABCBO = '1'
138400071204     C                   ENDIF
138500080225     C                   IF        SkSplitCSV(i) = 'A'
138600080225     C                   EVAL      VABCBO = '2'
138700080225     C                   ENDIF
138800080225     C                   ENDIF
138900071204     C*
139000070925     C*
139100071029     C***  <===  -----------------------------------------
139200040510     C*
139300030715     C                   ENDIF
139400030715     C                   EVAL      i = i + 1
139500030715     C                   ENDDO
139600070925     C*
139700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
139800080219     C*
139900080219     C* Considerazioni contrassegno
140000080219     C                   IF        VABCAS = *zeros
140100080219     C                   EVAL      VABTIC = *blanks
140200080219     C                   ENDIF
140300070925     C*
140400071029     C***  <===  -----------------------------------------
140500070925     C*
140600070925     C* Considerazioni finali su CBO/CAS
140700080219     C                   IF        VABCAS > *zeros
140800070925     C                   IF        VABCBO = '1'
140900070925     C                   EVAL      VABCBO = '4'
141000070925     C                   ENDIF
141100070925     C                   IF        VABCBO = '2'
141200070925     C                   EVAL      VABCBO = '6'
141300070925     C                   ENDIF
141400070925     C                   ENDIF
141500040510     C*
141600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
141700040510     C                   IF        VABNSP = *zeros
141800040510     C                   SETON                                        31
141900040510     C                   ENDIF
142000040715     C*
142100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
142200040715     C                   IF        VABLNP = *zeros
142300040715     C                   SETON                                        31
142400040715     C                   ENDIF
142500040510     C*
142600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
142700040510     C                   IF        VABRMN = *zeros
142800040510     C                   Z-ADD     VABNSP        VABRMN
142900040510     C                   ENDIF
143000080225     C*
143100080225     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
143200080225     C                   IF        VABRMA = *blanks
143300080225     C                   MOVE(P)   VABRMN        VABRMA
143400080225     C                   ENDIF
143500040128     C*
143600040510     C* Se provincia nn valorizzata la reperisco
143700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
143800040510     C                   IF        VABPRD = *blanks
143900040128     C                   CLEAR                   TISI95DS
144000040128     C                   EVAL      I95TCN = '3'
144100040128     C                   Z-ADD     datcor        I95DAT
144200040128     C                   EVAL      I95NAR = VABNZD
144300040128     C                   EVAL      I95CAP = VABCAD
144400040128     C                   EVAL      I95LOC = VABLOD
144500040128     C                   CALL      'TISI95R'
144600040128     C                   PARM                    TISI95DS
144700040128     C                   EVAL      VABPRD = O95PRV
144800040510     C                   ENDIF
144900040113     C*
145000040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
145100040113     C                   IF        VABCAS > *zeros
145200040119     C                   EVAL      VABVCA = 'EUR'
145300040119     C                   IF        VABCBO = '1'
145400040113     C                   EVAL      VABCBO = '4'
145500040119     C                   ENDIF
145600040119     C                   IF        VABCBO = '2'
145700040119     C                   EVAL      VABCBO = '6'
145800040119     C                   ENDIF
145900040113     C                   ENDIF
146000020204     C*
146100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
146200020204     C                   EXSR      CHKIMPDIV
146300030715     C                   ENDIF
146400020204     C*
146500010607     C                   ENDSR
146600010607     C*----------------------------------------------------*
146700010601
146800020204
146900020204     C*----------------------------------------------------*
147000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
147100020204     C*----------------------------------------------------*
147200020204     C     CHKIMPDIV     BEGSR
147300020204     C*
147400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
147500020204     C                   Z-ADD     *zeros        wrkDec            9 9
147600020204     C*
147700020204     C* Come prima cosa effettuo considerazioni sulla divisa
147800020204     C                   IF        vabIAS > *zeros
147900020204     C                   IF        vabVAS <> 'EUR'
148000020204     C                   EVAL      vabVAS =  'ITL'
148100020204     C                   ENDIF
148200020204     C                   ENDIF
148300020204     C*
148400020204     C                   IF        vabCAS > *zeros
148500020204     C                   IF        vabVCA <> 'EUR'
148600020204     C                   EVAL      vabVCA =  'ITL'
148700020204     C                   ENDIF
148800020204     C                   ENDIF
148900020204     C*
149000020204     C                   IF        vabVMD > *zeros
149100020204     C                   IF        vabVAD <> 'EUR'
149200020204     C                   EVAL      vabVAD =  'ITL'
149300020204     C                   ENDIF
149400020204     C                   ENDIF
149500020204     C*
149600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
149700020204     C                   Z-ADD     vabIAS        wrkDec
149800020204     C                   IF        wrkDec > *zeros
149900020204     C                   IF        vabVAS = 'ITL'
150000020204     C                   EVAL      vabIAS = *zeros
150100020204     C                   ENDIF
150200020204     C                   ENDIF
150300020204     C*
150400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
150500020204     C                   Z-ADD     vabCAS        wrkDec
150600020204     C                   IF        wrkDec > *zeros
150700020204     C                   IF        vabVCA = 'ITL'
150800020204     C                   EVAL      vabCAS = *zeros
150900020204     C                   ENDIF
151000020204     C                   ENDIF
151100020204     C*
151200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
151300020204     C                   Z-ADD     vabVMD        wrkDec
151400020204     C                   IF        wrkDec > *zeros
151500020204     C                   IF        vabVAD = 'ITL'
151600020204     C                   EVAL      vabVMD = *zeros
151700020204     C                   ENDIF
151800020204     C                   ENDIF
151900020204     C*
152000020204     C                   ENDSR
152100020204     C***
152200020204
152300010330
152400010330
152500010330     C*----------------------------------------------------*
152600010330     C*  CONTROLLO NUMERICITA' CAMPI
152700010330     C*----------------------------------------------------*
152800010330     C     CHKNUM        BEGSR
152900010330     C*
153000010606     C                   IF        PiDecChr = *blanks
153100030715     C                   EVAL      PiDecChr = CharNUM
153200010606     C                   ENDIF
153300010606     C*
153400010606     C                   CALL(e)   'ISNUMERIC'
153500010330     C                   PARM                    PiStr            30
153600010606     C                   PARM                    PiDecChr          1
153700010330     C                   PARM      *ZEROS        PiVal            30 9
153800010330     C                   PARM      '0'           PiInt             1
153900010330     C                   PARM      '0'           PiNum             1
154000010330     C                   IF        %error
154100010606     C                   EVAL      PiNum=*off
154200010330     C                   ENDIF
154300010330     C*
154400010330     C                   ENDSR
154500010330     C***
154600010330
154700010601
154800010601
154900010601
155000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
155100010601     C     repfil        BEGSR
155200010601     C*
155300010601     C                   if        invfil = *zeros and
155400010601     C                             depfil > *zeros and
155500010629     C                             (vinflg = *blanks or
155600010629     C                              vinflg = *zeros)
155700010601     C
155800010601     C                   eval      invfil = depfil
155900010601     C                   endif
156000010601     C*
156100010601     C                   if        depfil <> invfil and
156200010601     C                             invfil > *zeros
156300010601     C                   eval      flgMulti = '1'
156400010601     C                   if        vinflg = *blanks
156500010601     C                   add       1             cntNonEl
156600010601     C                   endif
156700010601     C                   endif
156800010601     C*
156900010601     C                   if        vinflg = '2'
157000010601     C                   eval      flgStato = '2'
157100010601     C                   endif
157200010601     C*
157300010601     C                   ENDSR
157400010601     C***
157500010601
157600010601
157700010601
157800010330
157900040119      /TITLE Invio dei dati al punto operativo.
158000040119     C     invio         BEGSR
158100040119     C*
158200040119     C* 1° invio FIVAT
158300040119     C                   reset                   dscmz
158400040119     C                   move      vlrpoi        cmzdst
158500040119     C                   eval      cmzfld = 'FIVATWWR'
158600040119     C                   eval      cmzmbd = vlrhdl
158700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
158800040119     C***                if        prmfir = *blanks
158900040119     C                   eval      cmzfla = 'FIVAT00F'
159000040119     C                   eval      cmzmba = 'FIVAT00F'
159100040119     C***                else
159200040119     C***                eval      cmzfla = prmfir
159300040119     C***                eval      cmzmba = prmfir
159400040119     C***                endif
159500040119     C                   eval      cmznrr = *zeros
159600040119     C                   move      §ctrokvt      cmznrr
159700040119     C                   eval      cmzlba = vlrfl1
159800040119     C                   call(e)   'TIS711C'
159900040119     C                   parm                    dscmz
160000040119     C                   parm      *blanks       esito
160100040119     C                   if        %error
160200040119     C                             or cmzerr = '1'
160300040119     C                             or esito  = '1'
160400040119     C                   eval      wrkesito = '3'
160500040119     C                   else
160600040119     C*
160700040119     C* 2° invio FIVAB
160800040119     C                   reset                   dscmz
160900040119     C                   move      vlrpoi        cmzdst
161000040119     C                   eval      cmzfld = vlrfou
161100040119     C                   eval      cmzmbd = vlrhdl
161200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161300040119     C***                if        prmfir = *blanks
161400040119     C                   eval      cmzfla = 'FIVAB00F'
161500040119     C                   eval      cmzmba = 'FIVAB00F'
161600040119     C***                else
161700040119     C***                eval      cmzfla = prmfir
161800040119     C***                eval      cmzmba = prmfir
161900040119     C***                endif
162000040119     C                   eval      cmznrr = *zeros
162100040119     C                   move      §ctrokvb      cmznrr
162200040119     C                   eval      cmzlba = vlrfl1
162300040119     C                   call(e)   'TIS711C'
162400040119     C                   parm                    dscmz
162500040119     C                   parm      *blanks       esito
162600040119     C                   if        %error
162700040119     C                             or cmzerr = '1'
162800040119     C                             or esito  = '1'
162900040119     C                   eval      wrkesito = '3'
163000040119     C                   endif
163100040119     C                   endif
163200040119     C*
163300040119     C                   ENDSR
163400040119     C***
163500010601
163600010601
163700010601
163800010601
163900010601      /TITLE Invio dei dati al punto operativo.
164000010601     C     opeini        BEGSR
164100010601     C*
164200010601     C* Inizializzo flag e contatori operativi
164300010601     C                   movel     '0'           flgGiro           1
164400010601     C                   movel     '0'           flgMulti          1
164500010601     C                   movel     '1'           flgStato          1
164600010615     C                   movel     '0'           flgOk             1
164700010601     C                   z-add     *zeros        cntNonEl         10 0
164800010601     C                   z-add     *zeros        depfil            3 0
164900010601     C                   z-add     *zeros        invfil            3 0
165000010601     C*
165100010601     C                   ENDSR
165200010601     C***
165300070326
165400070326
165500070326
165600070326
165700070326     C     *pssr         BEGSR
165800070329     C*
165900070329     C                   if        %open(tivin00r)
166000070329     C                   close     tivin00r
166100070329     C                   endif
166200070329     C                   if        %open(fivabwwr)
166300070329     C                   close     fivabwwr
166400070329     C                   endif
166500070329     C                   if        %open(fivatwwr)
166600070329     C                   close     fivatwwr
166700070329     C                   endif
166800070326     C*
166900070326     C* Effettuo la chiamata al CLLE preposto
167000070326     C                   call(e)   'TITVVTC'
167100070326     C                   parm                    parccm
167200070326     C                   parm                    parmbr
167300070326     C                   parm      '2'           paropz
167400070326     C*
167500070326     C                   eval      wrkesito = '2'
167600070404     C*
167700070404     C                   seton                                        LR
167800070326     C*
167900070326     C                   ENDSR     '*CANCL'
168000070326     C***
168100070326
168200070326
168300010330
168400010330
168500000613     C     *inzsr        BEGSR
168600990910     C*
168700990910     C     *entry        plist
168800990920     C                   parm                    tivlrds
168900990921     C                   parm      wrkesito      esito
169000000724     C                   parm                    prmlit
169100000710     C                   parm                    prmfir
169200010330     C*
169300010330     C* CALCOLA LA DATA CORRENTE
169400010330     C                   time                    wn14             14 0
169500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
169600010330     C                   z-add     wn8           g08dat
169700010330     C                   z-add     *zeros        g08inv
169800010330     C                   movel     '0'           g08err
169900010330     C                   call      'XSRDA8'
170000010330     C                   parm                    wlbda8
170100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
170200000613     C*
170300000613     C                   ENDSR
170400000613     C***
170500990908
170600080225     Otitv1e1p  E            riepilogo         2
170700990915     O                                              'Upload via Internet'
170800990915     O                                           +1 'Traduzione TIVIN00R -'
170900021025     O                                           +1 'FIVABWWR'
171000990915     O                                           +1 'Testate bolle'
171100990915     O          E            riepilogo   2
171200990915     O                       wrkdata
171300990915     O                       wrkora              +1
171400990915     O                       procname            +1
171500990915     O          E            riepilogo   2
171600990915     O                                              'Cliente..................:'
171700010605     O                       VABCCM        z     +1
171800990915     O          E            riepilogo   2
171900990920     O                                              'Riferimento Strategi.....:'
172000990920     O                       vlrhdl              +1
172100990915     O          E            riepilogo   2
172200990915     O                                              'Giusti...................:'
172300040119     O                       §CTROKVB      2   +  1
172400990915     O          E            riepilogo   2
172500990915     O                                              'Sbagliati e corretti.....:'
172600971022     O                       §CTRMO        2   +  1
172700990915     O          E            riepilogo   2
172800990915     O                                              'Sbagliati e scartati.....:'
172900971022     O                       §CTRNO        2   +  1
173000000613
173100080225     Otitv1e1ps E            testdett          2
173200000613     O                                              'Upload via Internet'
173300000613     O                                           +1 'Traduzione TIVIN00R -'
173400021025     O                                           +1 'FIVABWWR'
173500000613     O                                           +1 'Testate bolle'
173600000616     O          E            testdett    3
173700000613     O                                           +2 'N° rec'
173800000613     O                                           +3 'Anteprima contenuto'
173900000616     O          E            rigadett    2
174000000613     O                       rrnum               +2
174100000621     O                       recko               +3
174200000616     O          E            findett     2
174300000613     O                       wrkdata
174400000613     O                       wrkora              +1
174500000613     O                       procname            +1
174600000616     O          E            findett     2
174700000613     O                                              'Cliente..................:'
174800010605     O                       VABCCM        z     +1
174900000616     O          E            findett     2
175000000613     O                                              'Riferimento Strategi.....:'
175100000613     O                       vlrhdl              +1
175200000616     O          E            findett     2
175300000613     O                                              'Giusti...................:'
175400040119     O                       §CTROKVB      2   +  1
175500000616     O          E            findett     2
175600000613     O                                              'Sbagliati e corretti.....:'
175700000613     O                       §CTRMO        2   +  1
175800000616     O          E            findett     2
175900000613     O                                              'Sbagliati e scartati.....:'
176000000613     O                       §CTRNO        2   +  1
176100000613** CMD - COMANDI CL
176200080225OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1E1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
176300080225OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1E1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
176400080225DLTOVR FILE(TITV1E1P TITV1E1PS) LVL(*)
176500010606
176600000613
176700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
176800080225VABNSP;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;W_VABCBO;VABNCL;VABPKB
176900080225
177000080218
