000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800070823     Ftitv1a1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100070823     Ftitv1a1ps O    f  198        PRINTER usropn
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
004000040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
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
013300070823     C                   if        not %open(titv1a1ps)
013400070823     C                   open      titv1a1ps
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
016500070823     C                   if        %open(titv1a1ps)
016600000616     C                   except    findett
016700070823     C                   close     titv1a1ps
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
018700070823     C                   if        not %open(titv1a1p)
018800070823     C   50              open      titv1a1p
018900990915     C                   endif
019000990915     C*
019100030715     C   50              except    riepilogo
019200990915     C*
019300070823     C                   if        %open(titv1a1p)
019400070823     C   50              close     titv1a1p
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
023000070823     C                   exsr      inzvar
023100070823     C                   exsr      impvab
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
026700010330     C                   exsr      inzvar
026800010605     C                   exsr      impvab
026900010601     C*
027000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027100010605     C                   eval      depfil = VABLNP
027200010601     C                   exsr      repfil
027300010601     C                   if        depfil = invfil
027400021025     C                   if        vlrpoi = 999
027500030822     C                   move(P)   invfil        VABFGS
027600021025     C                   else
027700030822     C                   move(P)   vlrpoi        VABFGS
027800021025     C                   endif
027900010601     C*
028000010601     C                   exsr      PREELA
028100010601     C*
028200010604     C* Ebbene...
028300010604     C*
028400040119     C  N31              add       1             §CTROKVB
028500040119     C   32              add       1             §CTRMO
028600040119     C   31              add       1             §CTRNO
028700030822     C                   if        wGiro = 2
028800040510     C* Scrivo sempre il VAT
028900040510     C  N31              exsr      wrivat
029000040510     C* Scrivo il VAB solo a rottura d spedizione
029100040119     C                   if        depspe =  *zeros OR
029200040119     C                             depspe <> vabNSP
029300040119     C  N31              exsr      wrivab
029400040119     C  N31              eval      depspe = vabNSP
029500040119     C                   endif
029600040119     C                   endif
029700010604     C*
029800010604     C                   if        *in31 = *off and
029900010604     C                             *in32 = *off
030000010604     C                   eval      vinflg = '1'
030100010604     C                   else
030200010604     C                   eval      recko = vindta
030300010604     C                   except    rigadett
030400010604     C                   eval      vinflg = '2'
030500010604     C                   endif
030600010604     C                   endif
030700010601     C*
030800010604     C                   endif
030900010604     C*
031000010330     C                   else
031100010330     C                   eval      vinflg = '1'
031200010330     C                   endif
031300010601     C*
031400010601     C  N70              update    tivin000
031500010330     C*
031600030822     C  N70              enddo
031700040119     C*
031800040119     C                   endif                                                  (endif - chkcall)
031900010601     C*
032000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032100010601     C                   if        cntNonEl = *zeros or
032200010601     C                             flgMulti = '0'
032300010330     C* Se non ci sono record con errori ...
032400010601     C                   if        §ctrno = 0 and
032500010604     C                             §ctrmo = 0 and
032600010601     C                             flgStato <> '2'
032700010330     C* ... restituisco esito OK.
032800010330     C                   eval      wrkesito = '0'
032900010330     C                   else
033000040119     C                   if        §ctrokvb > 0
033100010330     C                   eval      wrkesito = '1'
033200010330     C                   else
033300010615     C                   if        flgOk = '0'
033400010615     C                   eval      wrkesito = '2'
033500010615     C                   else
033600010615     C                   eval      wrkesito = '6'
033700010615     C                   endif
033800010330     C                   endif
033900010330     C                   endif
034000010601     C                   else
034100010601     C                   eval      wrkesito = '9'
034200010601     C                   endif
034300010330     C*
034400010330     C                   if        %open(tivin00r)
034500010330     C                   close     tivin00r
034600010330     C                   endif
034700021025     C                   if        %open(fivabwwr)
034800021025     C                   close     fivabwwr
034900010330     C                   endif
035000040512     C                   if        %open(fivatwwr)
035100040512     C                   close     fivatwwr
035200040512     C                   endif
035300010601     C*
035400010601     C                   if        vlrpoi <> 999
035500010601     C                   eval      invfil = vlrpoi
035600010601     C                   endif
035700010330     C*
035800040119     C                   if        §ctrokvb > 0
035900010601     C                             and invfil > *zeros
036000010330     C                   exsr      invio
036100010330     C                   endif
036200010601     C*
036300010618     C                   if        flgGiro = '1'
036400010601     C                   exsr      endela
036500010618     C                   endif
036600010330     C*
036700010330     C                   ENDSR
036800010330     C***
036900040119
037000040119
037100040119
037200040119     C*----------------------------------------------------*
037300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037400040119     C*----------------------------------------------------*
037500040510     C     WRIVAB        BEGSR
037600040119     C*
037700060331     C* Gestisco l'eventuale rottura x numero spedizione
037800070823     C                   if        VABNSP <> depspe
037900070301     C                   if        wVATNOT_E <> *blanks
038000070301     C                   eval      VABCTM = '7Q'
038100070301     C                   endif
038200040119     C                   write     fivab000                                     => scarico il VAB
038300060331     C                   endif
038400040119     C*
038500040119     C                   ENDSR
038600040119
038700040119
038800040119
038900040119     C*----------------------------------------------------*
039000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039100040119     C*----------------------------------------------------*
039200040510     C     WRIVAT        BEGSR
039300060512     C*
039400060512     C* Inizializzo i campi chiave
039500060512     C                   eval      VATFGS = VABFGS
039600060512     C                   eval      VATAAS = VABAAS
039700060512     C                   eval      VATCCM = VABCCM
039800070823     C                   eval      VATNRS = VABNRS
039900060512     C                   eval      VATNSP = VABNSP
040000060512     C                   eval      VATLNP = VABLNP
040100040714     C*
040200040714     C* Solo se VATNOT valorizzato scarico il buffer
040300040714     C                   if        VATNOT <> *blanks
040400040119     C*
040500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040600040510     C                   if        VATTRC = *blanks
040700040510     C                   eval      VATTRC = 'E'
040800040510     C                   endif
040900040510     C                   write     FIVAT000
041000040119     C                   add       1             §CTROKVT
041100040714     C*
041200040714     C                   endif
041300060331     C*
041400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041500060331     C                   if        wVATNOT_A <> *blanks
041600060331     C                   eval      VATNOT = wVATNOT_A
041700060331     C                   eval      VATTRC = 'A'
041800060331     C                   write     FIVAT000
041900060331     C                   add       1             §CTROKVT
042000060331     C                   endif
042100060331     C*
042200060331     C                   if        wVATNOT_B <> *blanks
042300060331     C                   eval      VATNOT = wVATNOT_B
042400060331     C                   eval      VATTRC = 'B'
042500060331     C                   write     FIVAT000
042600060331     C                   add       1             §CTROKVT
042700060331     C                   endif
042800060331     C*
042900060331     C                   if        wVATNOT_E <> *blanks
043000060331     C                   eval      VATNOT = wVATNOT_E
043100060331     C                   eval      VATTRC = 'E'
043200060331     C                   write     FIVAT000
043300060331     C                   add       1             §CTROKVT
043400060331     C                   endif
043500040119     C*
043600040119     C                   ENDSR
043700040119
043800040119
043900040119
044000040119     C*----------------------------------------------------*
044100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044200040119     C*----------------------------------------------------*
044300040119     C     PREVAT        BEGSR
044400040119     C*
044500040119     C* Compongo il nome del membro da dare al FIVATWWR
044600040119     C                   eval      parmbr = vlrhdl
044700040119     C                   movel     'M'           parmbr
044800040510     C                   eval      parccm = vlrksc
044900040119     C                   eval      paropz = '1'
045000040119     C* Effettuo la chiamata al CLLE preposto
045100040119     C                   call(e)   'TITVVTC'
045200040119     C                   parm                    parccm
045300040119     C                   parm                    parmbr
045400040119     C                   parm                    paropz
045500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045600040119     C                   if        %error
045700040119     C                   movel     '1'           chkcall
045800040119     C                   else
045900040119     C                   movel     '0'           chkcall
046000040119     C                   endif
046100040119     C*
046200040119     C                   ENDSR
046300010601
046400010601
046500010601
046600010330     C*----------------------------------------------------*
046700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046800010330     C*----------------------------------------------------*
046900040119     C     INZVAR        BEGSR
047000010330     C*
047100030822     C* Inizializzo variabili di wrk
047200020204     C                   Z-ADD     *zeros        Num5_0            5 0
047300060331     C                   MOVEL     *blanks       wVATNOT_A
047400060331     C                   MOVEL     *blanks       wVATNOT_B
047500060331     C                   MOVEL     *blanks       wVATNOT_E
047600030822     C*
047700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047800030822     C                   CLEAR                   FIVAB000
047900040119     C                   CLEAR                   FIVAT000
048000030822     C                   CLEAR                   SkSplitCSV
048100030822     C                   EVAL      i = 1
048200030822     C                   EVAL      posDa = *zeros
048300030822     C                   EVAL      posA  = *zeros
048400030822     C*
048500030822     C* Reimposto i valori di default
048600030822     C                   EXSR      DEFCAM
048700030822     C*
048800010330     C                   ENDSR
048900010330     C*----------------------------------------------------*
049000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049100010330     C*----------------------------------------------------*
049200010330     C     DEFCAM        BEGSR
049300030822     C*
049400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049500030715     C* e delimitatore testo.
049600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049900030822     C*
050000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
050200030715     C                   SETON                                        50
050300030715     C                   ELSE
050400030715     C                   SETOFF                                       50
050500030715     C                   ENDIF
050600030822     C*
050700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050800030822     C                   EVAL      posDaDft = 1
050900030822     C                   EVAL      posADft  = 0
051000030822     C                   EVAL      wGiroDft = 0
051100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051200030822     C                             posDaDft > 0
051300030822     C*
051400030822     C* Gestisco il 1° giro
051500030822     C                   IF        wGiroDft = 0
051600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051800030822     C* Incremento il contatore dei "giri"
051900030822     C                   EVAL      wGiroDft = 1
052000030822     C                   ELSE
052100030822     C                   EVAL      posDaDft = posADft
052200030822     C                   ENDIF
052300030822     C* Eseguo lo scan x trovare la fine del campo corrente
052400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052500030822     C*
052600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052800030822     C                   IF        posDaDft > 0 AND
052900030822     C                             posADft  > 0
053000030822     C* NCL
053100030822     C                   IF        %subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):1:3)
053400030822     C                             = 'NCL'
053500030822     C                   EVAL      PiStr=%trim(%subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800030822     C                   EXSR      CHKNUM
053900030822     C                   IF        PiInt=*on
054000030822     C                   Z-ADD     PiVal         VABNCL
054100030822     C                   ENDIF
054200030822     C                   ENDIF
054300030822     C* CCM
054400030822     C                   IF        %subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):1:3)
054700030822     C                             = 'CCM'
054800030822     C                   EVAL      PiStr=%trim(%subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100030822     C                   EXSR      CHKNUM
055200030822     C                   IF        PiInt=*on
055300030822     C                   Z-ADD     PiVal         VABCCM
055400030822     C                   ENDIF
055500030822     C                   ENDIF
055600030822     C* LNP
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'LNP'
056100030822     C                   EVAL      PiStr=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   EXSR      CHKNUM
056500030822     C                   IF        PiInt=*on
056600030822     C                   Z-ADD     PiVal         VABLNP
056700030822     C                   ENDIF
056800030822     C                   ENDIF
056900030822     C* NRS
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'NRS'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900030822     C                   Z-ADD     PiVal         VABNRS
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* CTR
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'CTR'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiInt=*on
059200030822     C                   Z-ADD     PiVal         VABCTR
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* PKB
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'PKB'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiNum=*on
060500030822     C                   Z-ADD     PiVal         VABPKB
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* VLB
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'VLB'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiNum=*on
061800030822     C                   Z-ADD     PiVal         VABVLB
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* QFT
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'QFT'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiNum=*on
063100030822     C                   Z-ADD     PiVal         VABQFT
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* CBO
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'CBO'
063900030822     C                   EVAL      VABCBO=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   ENDIF
064300030822     C* TSP
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'TSP'
064800030822     C                   EVAL      VABTSP=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200030822     C* VAS
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'VAS'
065700030822     C                   EVAL      VABVAS=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* VCA
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'VCA'
066600030822     C                   EVAL      VABVCA=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* GCA
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'GCA'
067500030822     C                   EVAL      VABGCA=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* CTM
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'CTM'
068400030822     C                   EVAL      VABCTM=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* FFD
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'FFD'
069300030822     C                   EVAL      VABFFD=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* VAD
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'VAD'
070200030822     C                   EVAL      VABVAD=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* GMA
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'GMA'
071100030822     C                   EVAL      VABGMA=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* GGA
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'GGA'
072000030822     C                   EVAL      VABGGA=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* GVA
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'GVA'
072900030822     C                   EVAL      VABGVA=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* TC1
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'TC1'
073800030822     C                   EVAL      VABTC1=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* TC2
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'TC2'
074700030822     C                   EVAL      VABTC2=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100040714     C* VATTRC
075200040714     C                   IF        %subst(
075300040714     C                             %subst(vlrppt:posDaDft+1:
075400040714     C                             posADft-posDaDft-1):1:3)
075500040714     C                             = 'TRC'
075600040714     C                   EVAL      VATTRC=%trim(%subst(
075700040714     C                             %subst(vlrppt:posDaDft+1:
075800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900040714     C                   ENDIF
076000030822     C* ...
076100030822     C                   ENDIF
076200030822     C                   ENDDO
076300020204     C*
076400010330     C                   ENDSR
076500010607     C*----------------------------------------------------*
076600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
076700010607     C*----------------------------------------------------*
076800010607     C     IMPVAB        BEGSR
076900010607     C*
077000010607     C                   SETOFF                                       3132
077100030515     C*
077200010607     C                   MOVEL     datcor        VABAAS
077300010607     C                   MOVE      datcor        VABMGS
077400010607     C*
077500030715     C**********
077600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077700030715     C**********
077800030715     C                   IF        wGiro = *zeros
077900040113     C*
078000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
078100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
078200040510     C                             %trim(intcol(3))<>*blanks
078300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078400040113     C                                      %trim(intcol(3))
078500070823     C     ';':CharCSV   XLATE     vindta        vindta
078600040510     C                   ENDIF
078700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078800031201     C     minu:maiu     XLATE     vindta        vindta
078900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079000030715     C                   DOW       posDa <= %len(%trim(vindta))
079100030715     C*
079200030715     C* Gestisco il 1° campo
079300030715     C                   IF        i = 1
079400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079500030715     C                   EVAL      posDa = 1
079600030715     C* Eseguo lo scan x trovare la fine del primo campo
079700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079800030715     C* A questo punto "estraggo" il campo corrente
079900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080000030715     C                                                    (posA-posDa))
080100030715     C* X i campi successivi al 1°
080200030715     C                   ELSE
080300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080400030715     C                   EVAL      posDa = posA + 1
080500030715     C* Eseguo lo scan x trovare la fine del campo corrente
080600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080700030715     C* Gestisco l'ultimo campo
080800030715     C                   IF        posA = *zeros
080900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C                   LEAVE
081400030715     C                   ELSE
081500030715     C* A questo punto "estraggo" il campo corrente
081600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081700030715     C                                                    (posA-posDa))
081800030715     C                   ENDIF
081900030715     C                   ENDIF
082000030715     C* Incremento il contatore d campo
082100030715     C                   EVAL      i = i +1
082200030715     C                   ENDDO
082300030715     C*
082400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082500030715     C* delimitazione testo
082600030715     C                   EVAL      i = 1
082700030715     C                   DOW       i < %elem(SkSplitFLD)
082800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
083000030715     C                   EVAL      i = i + 1
083100030715     C                   ENDDO
083200030715     C                   EVAL      wGiro = 1
083300030715     C                   ELSE
083400030715     C                   EVAL      wGiro = 2
083500031204     C**********
083600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
083700031204     C* d separatore campo
083800031204     C**********
083900031204     C                   EVAL      vindta = %trim(vindta)
084000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084100031204     C                   EVAL      vindta = %subst(vindta:2)
084200031204     C                   ENDDO
084300031204     C*
084400031204     C                   Z-ADD     *zeros        lunghInput        4 0
084500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
084700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084900031204     C                   ENDDO
085000030715     C**********
085100030715     C* Effettuo lo split del campo dati d input
085200030715     C**********
085300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085400030715     C                   DOW       posDa <= %len(%trim(vindta))
085500030715     C*
085600030715     C* Gestisco il 1° campo
085700030715     C                   IF        i = 1
085800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085900030715     C                   EVAL      posDa = 1
086000030715     C* Eseguo lo scan x trovare la fine del primo campo
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C* X i campi successivi al 1°
086600030715     C                   ELSE
086700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086800030715     C                   EVAL      posDa = posA + 1
086900031204     C* Verifico che nn vi sia il campo nn valorizzato
087000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087100031204     C* Se campo nn valorizzato skippo al prossimo
087200031204     C                   EVAL      SkSplitCSV(i) = *blanks
087300031204     C                   EVAL      posA  = posA + 1
087400031204     C                   ELSE
087500030715     C* Eseguo lo scan x trovare la fine del campo corrente
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* Gestisco l'ultimo campo
087800030715     C                   IF        posA = *zeros
087900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   LEAVE
088400030715     C                   ELSE
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C                   ENDIF
088900031204     C                   ENDIF
089000030715     C                   ENDIF
089100030715     C* Incremento il contatore d campo
089200030715     C                   EVAL      i = i +1
089300030715     C                   ENDDO
089400030715     C*
089500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089600030715     C* delimitazione testo
089700030715     C                   EVAL      i = 1
089800030715     C                   DOW       i < %elem(SkSplitCSV)
089900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090100030715     C                   EVAL      i = i + 1
090200030715     C                   ENDDO
090300030715     C*
090400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090500030715     C                   EVAL      i = 1
090600030715     C                   DOW       i < %elem(SkSplitFLD)
090700030715     C*
090800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091000030715     C                             SkSplitCSV(i) <> *blanks
091100030721     C*** ATB
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
091300030721     C                   EVAL      VABATB = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030715     C*** RSD
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
091700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
091800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
092000030715     C* ==
092100030715     C                   ENDIF
092200030721     C*** RD2
092300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
092400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092500030721     C                   ENDIF
092600030715     C*** IND
092700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
092800030715     C                   EVAL      VABIND = SkSplitCSV(i)
092900030715     C                   ENDIF
093000030715     C*** LOD
093100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
093200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
093300030715     C                   ENDIF
093400030715     C*** PRD
093500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
093600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
093700030715     C                   ENDIF
093800030721     C*** NZD
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
094000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030715     C*** RMA
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
094400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094500030715     C                   ENDIF
094600030715     C*** NOT
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
094800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** NT2
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
095200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** VCA
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** VAS
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** FFD
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030721     C*** GC1
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
096800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*** GC2
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** TSP
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
097600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
097700030721     C                   ENDIF
097800040510     C*** CBO
097900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
098000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
098100040510     C                   ENDIF
098200030721     C*** NAS
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** TIC
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** GCA
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** XCO
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** CTM
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
100000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** TCR
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** CTS
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** FTM
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** VAD
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** GMA
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
102000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GGA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** GVA
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** TC1
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TC2
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** SCL
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
104000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** RMO
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
104400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** NMO
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*
105100030721     C* Reperisco quindi i campi numerici...
105200040510     C*** NSP
105300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105400040510     C                   EVAL      PiStr=SkSplitCSV(i)
105500040510     C                   EXSR      CHKNUM
105600040510     C                   IF        PiInt=*on
105700040510     C                   Z-ADD     PiVal         VABNSP
105800040510     C                   ELSE
105900040510     C                   SETON                                        31
106000040510     C                   EVAL      VABNSP = *zeros
106100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
106200040510     C                             + ' ' + 'VABNSP'
106300040510     C                   ENDIF
106400040510     C                   ENDIF
106500030721     C*** CAD
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiInt=*on
107000030721     C                   Z-ADD     PiVal         Num5_0
107100030721     C                   MOVEL(p)  Num5_0        VABCAD
107200030721     C                   ELSE
107300030721     C                   SETON                                        32
107400030721     C                   EVAL      VABCAD = *zeros
107500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107600030721     C                             + ' ' + 'VABCAD'
107700030721     C                   ENDIF
107800030721     C                   ENDIF
107900030721     C*** NCL
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108100030721     C                   EVAL      PiStr=SkSplitCSV(i)
108200030721     C                   EXSR      CHKNUM
108300030721     C                   IF        PiInt=*on
108400030721     C                   Z-ADD     PiVal         VABNCL
108500030721     C                   ELSE
108600030721     C                   SETON                                        32
108700030721     C                   EVAL      VABNCL = *zeros
108800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108900040122     C                             + ' ' + 'VABNCL'
109000030721     C                   ENDIF
109100030721     C                   ENDIF
109200030721     C*** PKB
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109400030721     C                   EVAL      PiStr=SkSplitCSV(i)
109500030721     C                   EXSR      CHKNUM
109600030721     C                   IF        PiNum=*on
109700030721     C                   Z-ADD     PiVal         VABPKB
109800030721     C                   ELSE
109900030721     C                   SETON                                        32
110000030721     C                   EVAL      VABPKB = *zeros
110100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110200030721     C                             + ' ' + 'VABPKB'
110300030721     C                   ENDIF
110400030721     C                   ENDIF
110500030721     C*** CAS
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiNum=*on
111000030721     C                   Z-ADD     PiVal         VABCAS
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      VABCAS = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500030721     C                             + ' ' + 'VABCAS'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** CCM
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on      AND
112300030721     C                             PiVal<=9999999 AND
112400030721     C                             PiVal>*zeros
112500030721     C                   Z-ADD     PiVal         VABCCM
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      VABCCM = *zeros
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000040510     C                             + ' ' + 'VABCCM'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** LNP
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiInt=*on    AND
113800030721     C                             PiVal<=999   AND
113900030721     C                             PiVal>*zeros
114000030721     C                   Z-ADD     PiVal         VABLNP
114100030721     C                   ELSE
114200040715     C                   SETON                                        32
114300030721     C                   EVAL      VABLNP = *zeros
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500040510     C                             + ' ' + 'VABLNP'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** NRS
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on AND
115300030721     C                             PiVal<=99
115400030721     C                   Z-ADD     PiVal         VABNRS
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABNRS'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** LNA
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on    AND
116600030721     C                             PiVal<=999   AND
116700030721     C                             PiVal>*zeros
116800030721     C                   Z-ADD     PiVal         VABLNA
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      VABLNA = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABLNA'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** CTR
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on  AND
118100030721     C                             PiVal<=999
118200030721     C                   Z-ADD     PiVal         VABCTR
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABCTR'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** IAS
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiNum=*on
119400030721     C                   Z-ADD     PiVal         VABIAS
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABIAS'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** VLB
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiNum=*on
120600030721     C                   Z-ADD     PiVal         VABVLB
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABVLB'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** QFT
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiNum=*on
121800030721     C                   Z-ADD     PiVal         VABQFT
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABQFT'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** NCD
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on      AND
123000030721     C                             PiVal<=9999999
123100030721     C                   Z-ADD     PiVal         VABNCD
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABNCD'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** NCA
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on      AND
124300030721     C                             PiVal<=9999999
124400030721     C                   Z-ADD     PiVal         VABNCA
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABNCA'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** ZNC
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on AND
125600030721     C                             PiVal<=99
125700030721     C                   Z-ADD     PiVal         VABZNC
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABZNC'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** DCR
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on       AND
126900030721     C                             PiVal<=99999999
127000030721     C                   Z-ADD     PiVal         VABDCR
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABDCR'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** HCR
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on   AND
128200030721     C                             PiVal<=9999
128300030721     C                   Z-ADD     PiVal         VABHCR
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABHCR'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** VMD
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiNum=*on
129500030721     C                   Z-ADD     PiVal         VABVMD
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABVMD'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** ANT
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on        AND
130700030721     C                             PiVal<=999999999
130800030721     C                   Z-ADD     PiVal         VABANT
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABANT'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** CMO
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on
132000030721     C                   Z-ADD     PiVal         Num5_0
132100030721     C                   MOVEL(p)  Num5_0        VABCMO
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      VABCMO = *zeros
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABCMO'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900040510     C*
133000040510     C* Reperisco i campi del FIVAT
133100040510     C*** VATNOT
133200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133400040510     C                   ENDIF
133500040510     C*** VATTRC
133600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133800040510     C                   ENDIF
133900060331     C*** VATNOT_A
134000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
134100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134200060331     C                   ENDIF
134300060331     C*** VATNOT_B
134400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134600060331     C                   ENDIF
134700060331     C*** VATNOT_E
134800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
134900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
135000060331     C                   ENDIF
135100070823     C*
135200070823     C* Gestione campi "particolari"
135300070823     C*** RSD_W
135400070823     C                   IF        %trim(SkSplitFLD(i)) = 'RSD_W'
135500070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
135600070823     C                   EVAL      VABRSD = %trim(SkSplitCSV(i))
135700070823     C                   ENDIF
135800070823     C                   ENDIF
135900070823     C*** IND_W
136000070823     C                   IF        %trim(SkSplitFLD(i)) = 'IND_W'
136100070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
136200070823     C                   EVAL      VABIND = %trim(SkSplitCSV(i))
136300070823     C                   ENDIF
136400070823     C                   ENDIF
136500070823     C*** LOD_W
136600070823     C                   IF        %trim(SkSplitFLD(i)) = 'LOD_W'
136700070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
136800070823     C                   EVAL      VABLOD = %trim(SkSplitCSV(i))
136900070823     C                   ENDIF
137000070823     C                   ENDIF
137100070823     C*** CAD_W
137200070823     C                   IF        %trim(SkSplitFLD(i)) = 'CAD_W'
137300070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
137400070823     C                   EVAL      VABCAD = %trim(SkSplitCSV(i))
137500070823     C                   ENDIF
137600070823     C                   ENDIF
137700070823     C*** PRD_W
137800070823     C                   IF        %trim(SkSplitFLD(i)) = 'PRD_W'
137900070823     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
138000070823     C                   EVAL      VABPRD = %trim(SkSplitCSV(i))
138100070823     C                   ENDIF
138200070823     C                   ENDIF
138300070823     C*** TIC_W
138400070823     C                   IF        %trim(SkSplitFLD(i)) = 'TIC_W'
138500070823     C                   IF        %trim(SkSplitCSV(i)) = '1'
138600070823     C                   EVAL      VABTIC = *blanks
138700070823     C                   ENDIF
138800070823     C                   IF        %trim(SkSplitCSV(i)) = '2'
138900070823     C                   EVAL      VABTIC = 'CM'
139000070823     C                   ENDIF
139100070823     C                   IF        %trim(SkSplitCSV(i)) = '3'
139200070823     C                   EVAL      VABTIC = 'BM'
139300070823     C                   ENDIF
139400070823     C                   ENDIF
139500070823     C*** TC1_W
139600070823     C                   IF        %trim(SkSplitFLD(i)) = 'TC1_W'
139700070823     C                   IF        %trim(SkSplitCSV(i)) = 'SI'
139800070823     C                   EVAL      VABTC1 = 'P'
139900070823     C                   ENDIF
140000070823     C                   ENDIF
140100070823     C*** TC2_W
140200070823     C                   IF        %trim(SkSplitFLD(i)) = 'TC2_W'
140300070823     C                   IF        %trim(SkSplitCSV(i)) = 'SI'
140400070823     C                   EVAL      VABTC2 = 'A'
140500070823     C                   ENDIF
140600070823     C                   ENDIF
140700070823     C*** VABNSP
140800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
140900070823     C                   EVAL      PiStr=SkSplitCSV(i)
141000070823     C                   EXSR      CHKNUM
141100070823     C                   IF        PiInt=*on
141200070823     C                   Z-ADD     PiVal         VABNSP
141300070823     C                   ELSE
141400070823     C                   SETON                                        31
141500070823     C                   EVAL      VABNSP = *zeros
141600070823     C                   EVAL      vinmsg = %trimr(vinmsg)
141700070823     C                             + ' ' + 'VABNSP'
141800070823     C                   ENDIF
141900070823     C                   ENDIF
142000070823     C*** VABDCR
142100070823     C                   IF        %trim(SkSplitFLD(i)) = 'DCR_W'
142200070823     C                   EVAL      PiStr=SkSplitCSV(i)
142300070823     C                   EVAL      PiStr=%subst(PiStr:7:4)+%subst(PiStr:4:2)+
142400070823     C                                   %subst(PiStr:1:2)
142500070823     C                   EXSR      CHKNUM
142600070823     C                   IF        PiInt=*on       AND
142700070823     C                             PiVal<=99999999
142800070823     C                   Z-ADD     PiVal         VABDCR
142900070823     C                   ELSE
143000070823     C                   SETON                                        32
143100070823     C                   EVAL      vinmsg = %trimr(vinmsg)
143200070823     C                             + ' ' + 'VABDCR'
143300070823     C                   ENDIF
143400070823     C                   ENDIF
143500040510     C*
143600030715     C                   ENDIF
143700030715     C                   EVAL      i = i + 1
143800030715     C                   ENDDO
143900040510     C*
144000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
144100040510     C                   IF        VABNSP = *zeros
144200040510     C                   SETON                                        31
144300040510     C                   ENDIF
144400040715     C*
144500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
144600040715     C                   IF        VABLNP = *zeros
144700040715     C                   SETON                                        31
144800040715     C                   ENDIF
144900040510     C*
145000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
145100040510     C                   IF        VABRMN = *zeros
145200040510     C                   Z-ADD     VABNSP        VABRMN
145300040510     C                   ENDIF
145400040510     C*
145500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
145600040510     C                   IF        VABRMA = *blanks
145700040714     C                   MOVEL     VABRMN        VABRMA
145800040510     C                   ENDIF
145900040128     C*
146000040510     C* Se provincia nn valorizzata la reperisco
146100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146200040510     C                   IF        VABPRD = *blanks
146300040128     C                   CLEAR                   TISI95DS
146400040128     C                   EVAL      I95TCN = '3'
146500040128     C                   Z-ADD     datcor        I95DAT
146600040128     C                   EVAL      I95NAR = VABNZD
146700040128     C                   EVAL      I95CAP = VABCAD
146800040128     C                   EVAL      I95LOC = VABLOD
146900040128     C                   CALL      'TISI95R'
147000040128     C                   PARM                    TISI95DS
147100040128     C                   EVAL      VABPRD = O95PRV
147200040510     C                   ENDIF
147300040113     C*
147400040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
147500040113     C                   IF        VABCAS > *zeros
147600040119     C                   EVAL      VABVCA = 'EUR'
147700040119     C                   IF        VABCBO = '1'
147800040113     C                   EVAL      VABCBO = '4'
147900040119     C                   ENDIF
148000040119     C                   IF        VABCBO = '2'
148100040119     C                   EVAL      VABCBO = '6'
148200040119     C                   ENDIF
148300040113     C                   ENDIF
148400020204     C*
148500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
148600020204     C                   EXSR      CHKIMPDIV
148700030715     C                   ENDIF
148800020204     C*
148900010607     C                   ENDSR
149000010607     C*----------------------------------------------------*
149100010601
149200020204
149300020204     C*----------------------------------------------------*
149400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149500020204     C*----------------------------------------------------*
149600020204     C     CHKIMPDIV     BEGSR
149700020204     C*
149800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149900020204     C                   Z-ADD     *zeros        wrkDec            9 9
150000020204     C*
150100020204     C* Come prima cosa effettuo considerazioni sulla divisa
150200020204     C                   IF        vabIAS > *zeros
150300020204     C                   IF        vabVAS <> 'EUR'
150400020204     C                   EVAL      vabVAS =  'ITL'
150500020204     C                   ENDIF
150600020204     C                   ENDIF
150700020204     C*
150800020204     C                   IF        vabCAS > *zeros
150900020204     C                   IF        vabVCA <> 'EUR'
151000020204     C                   EVAL      vabVCA =  'ITL'
151100020204     C                   ENDIF
151200020204     C                   ENDIF
151300020204     C*
151400020204     C                   IF        vabVMD > *zeros
151500020204     C                   IF        vabVAD <> 'EUR'
151600020204     C                   EVAL      vabVAD =  'ITL'
151700020204     C                   ENDIF
151800020204     C                   ENDIF
151900020204     C*
152000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152100020204     C                   Z-ADD     vabIAS        wrkDec
152200020204     C                   IF        wrkDec > *zeros
152300020204     C                   IF        vabVAS = 'ITL'
152400020204     C                   EVAL      vabIAS = *zeros
152500020204     C                   ENDIF
152600020204     C                   ENDIF
152700020204     C*
152800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152900020204     C                   Z-ADD     vabCAS        wrkDec
153000020204     C                   IF        wrkDec > *zeros
153100020204     C                   IF        vabVCA = 'ITL'
153200020204     C                   EVAL      vabCAS = *zeros
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153700020204     C                   Z-ADD     vabVMD        wrkDec
153800020204     C                   IF        wrkDec > *zeros
153900020204     C                   IF        vabVAD = 'ITL'
154000020204     C                   EVAL      vabVMD = *zeros
154100020204     C                   ENDIF
154200020204     C                   ENDIF
154300020204     C*
154400020204     C                   ENDSR
154500020204     C***
154600020204
154700010330
154800010330
154900010330     C*----------------------------------------------------*
155000010330     C*  CONTROLLO NUMERICITA' CAMPI
155100010330     C*----------------------------------------------------*
155200010330     C     CHKNUM        BEGSR
155300010330     C*
155400010606     C                   IF        PiDecChr = *blanks
155500030715     C                   EVAL      PiDecChr = CharNUM
155600010606     C                   ENDIF
155700010606     C*
155800010606     C                   CALL(e)   'ISNUMERIC'
155900010330     C                   PARM                    PiStr            30
156000010606     C                   PARM                    PiDecChr          1
156100010330     C                   PARM      *ZEROS        PiVal            30 9
156200010330     C                   PARM      '0'           PiInt             1
156300010330     C                   PARM      '0'           PiNum             1
156400010330     C                   IF        %error
156500010606     C                   EVAL      PiNum=*off
156600010330     C                   ENDIF
156700010330     C*
156800010330     C                   ENDSR
156900010330     C***
157000010330
157100010601
157200010601
157300010601
157400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157500010601     C     repfil        BEGSR
157600010601     C*
157700010601     C                   if        invfil = *zeros and
157800010601     C                             depfil > *zeros and
157900010629     C                             (vinflg = *blanks or
158000010629     C                              vinflg = *zeros)
158100010601     C
158200010601     C                   eval      invfil = depfil
158300010601     C                   endif
158400010601     C*
158500010601     C                   if        depfil <> invfil and
158600010601     C                             invfil > *zeros
158700010601     C                   eval      flgMulti = '1'
158800010601     C                   if        vinflg = *blanks
158900010601     C                   add       1             cntNonEl
159000010601     C                   endif
159100010601     C                   endif
159200010601     C*
159300010601     C                   if        vinflg = '2'
159400010601     C                   eval      flgStato = '2'
159500010601     C                   endif
159600010601     C*
159700010601     C                   ENDSR
159800010601     C***
159900010601
160000010601
160100010601
160200010330
160300040119      /TITLE Invio dei dati al punto operativo.
160400040119     C     invio         BEGSR
160500040119     C*
160600040119     C* 1° invio FIVAT
160700040119     C                   reset                   dscmz
160800040119     C                   move      vlrpoi        cmzdst
160900040119     C                   eval      cmzfld = 'FIVATWWR'
161000040119     C                   eval      cmzmbd = vlrhdl
161100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161200040119     C***                if        prmfir = *blanks
161300040119     C                   eval      cmzfla = 'FIVAT00F'
161400040119     C                   eval      cmzmba = 'FIVAT00F'
161500040119     C***                else
161600040119     C***                eval      cmzfla = prmfir
161700040119     C***                eval      cmzmba = prmfir
161800040119     C***                endif
161900040119     C                   eval      cmznrr = *zeros
162000040119     C                   move      §ctrokvt      cmznrr
162100040119     C                   eval      cmzlba = vlrfl1
162200040119     C                   call(e)   'TIS711C'
162300040119     C                   parm                    dscmz
162400040119     C                   parm      *blanks       esito
162500040119     C                   if        %error
162600040119     C                             or cmzerr = '1'
162700040119     C                             or esito  = '1'
162800040119     C                   eval      wrkesito = '3'
162900040119     C                   else
163000040119     C*
163100040119     C* 2° invio FIVAB
163200040119     C                   reset                   dscmz
163300040119     C                   move      vlrpoi        cmzdst
163400040119     C                   eval      cmzfld = vlrfou
163500040119     C                   eval      cmzmbd = vlrhdl
163600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163700040119     C***                if        prmfir = *blanks
163800040119     C                   eval      cmzfla = 'FIVAB00F'
163900040119     C                   eval      cmzmba = 'FIVAB00F'
164000040119     C***                else
164100040119     C***                eval      cmzfla = prmfir
164200040119     C***                eval      cmzmba = prmfir
164300040119     C***                endif
164400040119     C                   eval      cmznrr = *zeros
164500040119     C                   move      §ctrokvb      cmznrr
164600040119     C                   eval      cmzlba = vlrfl1
164700040119     C                   call(e)   'TIS711C'
164800040119     C                   parm                    dscmz
164900040119     C                   parm      *blanks       esito
165000040119     C                   if        %error
165100040119     C                             or cmzerr = '1'
165200040119     C                             or esito  = '1'
165300040119     C                   eval      wrkesito = '3'
165400040119     C                   endif
165500040119     C                   endif
165600040119     C*
165700040119     C                   ENDSR
165800040119     C***
165900010601
166000010601
166100010601
166200010601
166300010601      /TITLE Invio dei dati al punto operativo.
166400010601     C     opeini        BEGSR
166500010601     C*
166600010601     C* Inizializzo flag e contatori operativi
166700010601     C                   movel     '0'           flgGiro           1
166800010601     C                   movel     '0'           flgMulti          1
166900010601     C                   movel     '1'           flgStato          1
167000010615     C                   movel     '0'           flgOk             1
167100010601     C                   z-add     *zeros        cntNonEl         10 0
167200010601     C                   z-add     *zeros        depfil            3 0
167300010601     C                   z-add     *zeros        invfil            3 0
167400010601     C*
167500010601     C                   ENDSR
167600010601     C***
167700070326
167800070326
167900070326
168000070326
168100070326     C     *pssr         BEGSR
168200070329     C*
168300070329     C                   if        %open(tivin00r)
168400070329     C                   close     tivin00r
168500070329     C                   endif
168600070329     C                   if        %open(fivabwwr)
168700070329     C                   close     fivabwwr
168800070329     C                   endif
168900070329     C                   if        %open(fivatwwr)
169000070329     C                   close     fivatwwr
169100070329     C                   endif
169200070326     C*
169300070326     C* Effettuo la chiamata al CLLE preposto
169400070326     C                   call(e)   'TITVVTC'
169500070326     C                   parm                    parccm
169600070326     C                   parm                    parmbr
169700070326     C                   parm      '2'           paropz
169800070326     C*
169900070326     C                   eval      wrkesito = '2'
170000070404     C*
170100070404     C                   seton                                        LR
170200070326     C*
170300070326     C                   ENDSR     '*CANCL'
170400070326     C***
170500070326
170600070326
170700010330
170800010330
170900000613     C     *inzsr        BEGSR
171000990910     C*
171100990910     C     *entry        plist
171200990920     C                   parm                    tivlrds
171300990921     C                   parm      wrkesito      esito
171400000724     C                   parm                    prmlit
171500000710     C                   parm                    prmfir
171600010330     C*
171700010330     C* CALCOLA LA DATA CORRENTE
171800010330     C                   time                    wn14             14 0
171900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
172000010330     C                   z-add     wn8           g08dat
172100010330     C                   z-add     *zeros        g08inv
172200010330     C                   movel     '0'           g08err
172300010330     C                   call      'XSRDA8'
172400010330     C                   parm                    wlbda8
172500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
172600000613     C*
172700000613     C                   ENDSR
172800000613     C***
172900990908
173000070823     Otitv1a1p  E            riepilogo         2
173100990915     O                                              'Upload via Internet'
173200990915     O                                           +1 'Traduzione TIVIN00R -'
173300021025     O                                           +1 'FIVABWWR'
173400990915     O                                           +1 'Testate bolle'
173500990915     O          E            riepilogo   2
173600990915     O                       wrkdata
173700990915     O                       wrkora              +1
173800990915     O                       procname            +1
173900990915     O          E            riepilogo   2
174000990915     O                                              'Cliente..................:'
174100010605     O                       VABCCM        z     +1
174200990915     O          E            riepilogo   2
174300990920     O                                              'Riferimento Strategi.....:'
174400990920     O                       vlrhdl              +1
174500990915     O          E            riepilogo   2
174600990915     O                                              'Giusti...................:'
174700040119     O                       §CTROKVB      2   +  1
174800990915     O          E            riepilogo   2
174900990915     O                                              'Sbagliati e corretti.....:'
175000971022     O                       §CTRMO        2   +  1
175100990915     O          E            riepilogo   2
175200990915     O                                              'Sbagliati e scartati.....:'
175300971022     O                       §CTRNO        2   +  1
175400000613
175500070823     Otitv1a1ps E            testdett          2
175600000613     O                                              'Upload via Internet'
175700000613     O                                           +1 'Traduzione TIVIN00R -'
175800021025     O                                           +1 'FIVABWWR'
175900000613     O                                           +1 'Testate bolle'
176000000616     O          E            testdett    3
176100000613     O                                           +2 'N° rec'
176200000613     O                                           +3 'Anteprima contenuto'
176300000616     O          E            rigadett    2
176400000613     O                       rrnum               +2
176500000621     O                       recko               +3
176600000616     O          E            findett     2
176700000613     O                       wrkdata
176800000613     O                       wrkora              +1
176900000613     O                       procname            +1
177000000616     O          E            findett     2
177100000613     O                                              'Cliente..................:'
177200010605     O                       VABCCM        z     +1
177300000616     O          E            findett     2
177400000613     O                                              'Riferimento Strategi.....:'
177500000613     O                       vlrhdl              +1
177600000616     O          E            findett     2
177700000613     O                                              'Giusti...................:'
177800040119     O                       §CTROKVB      2   +  1
177900000616     O          E            findett     2
178000000613     O                                              'Sbagliati e corretti.....:'
178100000613     O                       §CTRMO        2   +  1
178200000616     O          E            findett     2
178300000613     O                                              'Sbagliati e scartati.....:'
178400000613     O                       §CTRNO        2   +  1
178500000613** CMD - COMANDI CL
178600070823OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1A1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
178700070823OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1A1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
178800070823DLTOVR FILE(TITV1A1P TITV1A1PS) LVL(*)
178900010606
179000000613
179100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179200070823VABRSD;VABRD2;VABIND;VABLOD;VABCAD;VABPRD;RSD_W;IND_W;LOD_W;CAD_W;VABNOT;
179300070823VABNT2;VABCAS;TIC_W;VABIAS;VABCBO;TC1_W;TC2_W;DCR_W;FILL1;FILL2;VABRMN;
179400070823FILL3;VABNCL;VABPKB;FILL4;FILL5;FILL6;FILL7;FILL8;FILL9;VATNOT_E
