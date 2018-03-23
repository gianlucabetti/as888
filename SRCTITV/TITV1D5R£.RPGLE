000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800080125     Ftitv1d5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100080125     Ftitv1d5ps O    f  198        PRINTER usropn
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
004500080125     D wFlgCAS         s              1    INZ(*blanks)
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
007600031201     D*------------------
007700031201     D* Costanti
007800031201     D*------------------
007900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008100010330
008200010330
008300010330
008400990915     C                   time                    wrkdata
008500990915     C                   time                    wrkora
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800990921     C                   reset                   wrkesito
008900010601     C*
009000010601     C                   exsr      opeini
009100010605     C                   exsr      rwvab
009200010601     C*
009300010601     C                   seton                                        lr
009400010601
009500010601
009600010601
009700010601     C*--------------------------------------------------------
009800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009900010601     C*--------------------------------------------------------
010000010601     C     PREELA        BEGSR
010100010601     C*
010200010601     C* SE OCCORRE SPEDIRE IN FILIALE
010300010601     C                   if        invfil <> *zeros and
010400010601     C                             flgGiro = '0'
010500010601     C*
010600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010601     C                   eval      flgGiro = '1'
010800000724     C*
010900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100010601     C     invfil        chain     azorg01l
011200000724     C                   if        %found
011300000724     C                   movel(p)  CMD(1)        depcmd
011400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011500000724     C*
011600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011700011113     C                   Z-ADD     150           LENGH            15 5
011800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000724     C                   PARM                    depcmd
012000000724     C                   PARM                    LENGH
012100000724     C*
012200000724     C                   endif
012300000616     C*
012400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500000616     C                   movel(p)  CMD(2)        depcmd
012600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012700000616     C*
012800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012900011113     C                   Z-ADD     150           LENGH            15 5
013000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013100000616     C                   PARM                    depcmd
013200000616     C                   PARM                    LENGH
013300000613     C*
013400080125     C                   if        not %open(titv1d5ps)
013500080125     C                   open      titv1d5ps
013600070329     C                   except    testdett
013700000613     C                   endif
013800010601     C*
013900010601     C                   endif
014000010601     C*
014100010601     C                   ENDSR
014200010601     C***
014300010601
014400010601
014500010601
014600010601
014700010601     C*--------------------------------------------------------
014800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014900010601     C*--------------------------------------------------------
015000010601     C     ENDELA        BEGSR
015100990915     C*
015200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015300040119     C*
015400040119     C* Effettuo la chiamata al CLLE preposto
015500040119     C                   call(e)   'TITVVTC'
015600040510     C                   parm                    parccm
015700040119     C                   parm                    parmbr
015800040119     C                   parm      '2'           paropz
015900050201     C*
016000050201     C* Effettuo lancio TISI95 solo x chiusura
016100050201     C                   CLEAR                   TISI95DS
016200050201     C                   EVAL      I95TLA = 'C'
016300050201     C                   CALL      'TISI95R'
016400050201     C                   PARM                    TISI95DS
016500000613     C*
016600080125     C                   if        %open(titv1d5ps)
016700000616     C                   except    findett
016800080125     C                   close     titv1d5ps
016900000613     C                   endif
017000000616     C*
017100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017200011113     C                   Z-ADD     150           LENGH            15 5
017300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017400000616     C                   PARM                    CMD(3)
017500000616     C                   PARM                    LENGH
017600000616     C*
017700010601     C                   ENDSR
017800010601     C***
017900010601
018000010601
018100010601
018200000613
018300000613     C*--------------------------------------------------------
018400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018500000613     C*--------------------------------------------------------
018600000621     C     STPR          BEGSR
018700000613     C*
018800080125     C                   if        not %open(titv1d5p)
018900080125     C   50              open      titv1d5p
019000990915     C                   endif
019100990915     C*
019200030715     C   50              except    riepilogo
019300990915     C*
019400080125     C                   if        %open(titv1d5p)
019500080125     C   50              close     titv1d5p
019600990914     C                   endif
019700000613     C*
019800000613     C                   ENDSR
019900000613     C***
020000000613
020100010601
020200010601
020300010330     C*--------------------------------------------------------
020400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020500010330     C*--------------------------------------------------------
020600010605     C     RWVAB         BEGSR
020700010330     C*
020800010330     C                   if        not %open(tivin00r)
020900010330     C                   open      tivin00r
021000010330     C                   endif
021100021025     C                   if        not %open(fivabwwr)
021200021025     C                   open      fivabwwr
021300010330     C                   endif
021400040119     C*
021500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021600040119     C                   exsr      prevat
021700040119     C*
021800040119     C                   if        chkcall = '0'
021900040119     C*
022000040119     C                   if        not %open(fivatwwr)
022100040119     C                   open      fivatwwr
022200040119     C                   endif
022300010330     C*
022400040119     C                   clear                   §CTROKVB          7 0
022500040119     C                   clear                   §CTROKVT          7 0
022600040119     C                   clear                   §CTRMO            7 0
022700040119     C                   clear                   §CTRNO            7 0
022800070823     C*
022900070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
023000070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
023100080108     C***                exsr      inzvar
023200080108     C***                exsr      impvab
023300010330     C*
023400030822     C                   do        *HIVAL
023500010330     C*
023600030822     C                   read      tivin00r                               70
023700010618     C*
023800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023900010618     C                   if        vinflg = '1'
024000010618     C                   eval      flgOk = '1'
024100010618     C                   endif
024200040510     C*
024300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024400040510     C                   z-add     *zeros        wLenVINDTA        4 0
024500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024900040510     C                   z-add     1             wFlgVINDTA
025000040510     C                   leave
025100040510     C                   endif
025200040510     C                   enddo
025300010618     C*
025400040510     C                   if        vindta > *blanks AND
025500040510     C                             wFlgVINDTA = 1
025600010330     C                   add       1             rrnum
025700010330     C*
025800010601     C                   if        *in70 = *off and
025900010330     C                             (vinflg = *blanks
026000010330     C                              or vinflg = '0'
026100010330     C                              or vinflg = '2')
026200010330     C*
026300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026400010711     C                   if        vinflg = *blanks or vinflg = '0'
026500010711     C                   clear                   vinmsg
026600010711     C                   endif
026700010601     C*
026800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
026900070925     C                   eval      vindta  = ' ' + %trim(vindta)
027000070925     C*
027100010330     C                   exsr      inzvar
027200010605     C                   exsr      impvab
027300010601     C*
027400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027500010605     C                   eval      depfil = VABLNP
027600010601     C                   exsr      repfil
027700010601     C                   if        depfil = invfil
027800021025     C                   if        vlrpoi = 999
027900030822     C                   move(P)   invfil        VABFGS
028000021025     C                   else
028100030822     C                   move(P)   vlrpoi        VABFGS
028200021025     C                   endif
028300010601     C*
028400010601     C                   exsr      PREELA
028500010601     C*
028600010604     C* Ebbene...
028700010604     C*
028800040119     C  N31              add       1             §CTROKVB
028900040119     C   32              add       1             §CTRMO
029000040119     C   31              add       1             §CTRNO
029100030822     C                   if        wGiro = 2
029200040510     C* Scrivo sempre il VAT
029300040510     C  N31              exsr      wrivat
029400040510     C* Scrivo il VAB solo a rottura d spedizione
029500040119     C                   if        depspe =  *zeros OR
029600040119     C                             depspe <> vabNSP
029700040119     C  N31              exsr      wrivab
029800040119     C  N31              eval      depspe = vabNSP
029900040119     C                   endif
030000040119     C                   endif
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
032200040119     C                   endif                                                  (endif - chkcall)
032300010601     C*
032400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032500010601     C                   if        cntNonEl = *zeros or
032600010601     C                             flgMulti = '0'
032700010330     C* Se non ci sono record con errori ...
032800010601     C                   if        §ctrno = 0 and
032900010604     C                             §ctrmo = 0 and
033000010601     C                             flgStato <> '2'
033100010330     C* ... restituisco esito OK.
033200010330     C                   eval      wrkesito = '0'
033300010330     C                   else
033400040119     C                   if        §ctrokvb > 0
033500010330     C                   eval      wrkesito = '1'
033600010330     C                   else
033700010615     C                   if        flgOk = '0'
033800010615     C                   eval      wrkesito = '2'
033900010615     C                   else
034000010615     C                   eval      wrkesito = '6'
034100010615     C                   endif
034200010330     C                   endif
034300010330     C                   endif
034400010601     C                   else
034500010601     C                   eval      wrkesito = '9'
034600010601     C                   endif
034700010330     C*
034800010330     C                   if        %open(tivin00r)
034900010330     C                   close     tivin00r
035000010330     C                   endif
035100021025     C                   if        %open(fivabwwr)
035200021025     C                   close     fivabwwr
035300010330     C                   endif
035400040512     C                   if        %open(fivatwwr)
035500040512     C                   close     fivatwwr
035600040512     C                   endif
035700010601     C*
035800010601     C                   if        vlrpoi <> 999
035900010601     C                   eval      invfil = vlrpoi
036000010601     C                   endif
036100010330     C*
036200040119     C                   if        §ctrokvb > 0
036300010601     C                             and invfil > *zeros
036400010330     C                   exsr      invio
036500010330     C                   endif
036600010601     C*
036700010618     C                   if        flgGiro = '1'
036800010601     C                   exsr      endela
036900010618     C                   endif
037000010330     C*
037100010330     C                   ENDSR
037200010330     C***
037300040119
037400040119
037500040119
037600040119     C*----------------------------------------------------*
037700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037800040119     C*----------------------------------------------------*
037900040510     C     WRIVAB        BEGSR
038000040119     C*
038100060331     C* Gestisco l'eventuale rottura x numero spedizione
038200070823     C                   if        VABNSP <> depspe
038300070301     C                   if        wVATNOT_E <> *blanks
038400080125     C                   if        VABCTM = *blanks
038500080125     C                   eval      VABCTM = '7Q'
038600080125     C                   endif
038700070301     C                   endif
038800040119     C                   write     fivab000                                     => scarico il VAB
038900060331     C                   endif
039000040119     C*
039100040119     C                   ENDSR
039200040119
039300040119
039400040119
039500040119     C*----------------------------------------------------*
039600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039700040119     C*----------------------------------------------------*
039800040510     C     WRIVAT        BEGSR
039900060512     C*
040000060512     C* Inizializzo i campi chiave
040100060512     C                   eval      VATFGS = VABFGS
040200060512     C                   eval      VATAAS = VABAAS
040300060512     C                   eval      VATCCM = VABCCM
040400070823     C                   eval      VATNRS = VABNRS
040500060512     C                   eval      VATNSP = VABNSP
040600060512     C                   eval      VATLNP = VABLNP
040700040714     C*
040800040714     C* Solo se VATNOT valorizzato scarico il buffer
040900040714     C                   if        VATNOT <> *blanks
041000040119     C*
041100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
041200040510     C                   if        VATTRC = *blanks
041300040510     C                   eval      VATTRC = 'E'
041400040510     C                   endif
041500040510     C                   write     FIVAT000
041600040119     C                   add       1             §CTROKVT
041700040714     C*
041800040714     C                   endif
041900060331     C*
042000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042100060331     C                   if        wVATNOT_A <> *blanks
042200060331     C                   eval      VATNOT = wVATNOT_A
042300060331     C                   eval      VATTRC = 'A'
042400060331     C                   write     FIVAT000
042500060331     C                   add       1             §CTROKVT
042600060331     C                   endif
042700060331     C*
042800060331     C                   if        wVATNOT_B <> *blanks
042900060331     C                   eval      VATNOT = wVATNOT_B
043000060331     C                   eval      VATTRC = 'B'
043100060331     C                   write     FIVAT000
043200060331     C                   add       1             §CTROKVT
043300060331     C                   endif
043400060331     C*
043500060331     C                   if        wVATNOT_E <> *blanks
043600060331     C                   eval      VATNOT = wVATNOT_E
043700060331     C                   eval      VATTRC = 'E'
043800060331     C                   write     FIVAT000
043900060331     C                   add       1             §CTROKVT
044000060331     C                   endif
044100040119     C*
044200040119     C                   ENDSR
044300040119
044400040119
044500040119
044600040119     C*----------------------------------------------------*
044700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044800040119     C*----------------------------------------------------*
044900040119     C     PREVAT        BEGSR
045000040119     C*
045100040119     C* Compongo il nome del membro da dare al FIVATWWR
045200040119     C                   eval      parmbr = vlrhdl
045300040119     C                   movel     'M'           parmbr
045400040510     C                   eval      parccm = vlrksc
045500040119     C                   eval      paropz = '1'
045600040119     C* Effettuo la chiamata al CLLE preposto
045700040119     C                   call(e)   'TITVVTC'
045800040119     C                   parm                    parccm
045900040119     C                   parm                    parmbr
046000040119     C                   parm                    paropz
046100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046200040119     C                   if        %error
046300040119     C                   movel     '1'           chkcall
046400040119     C                   else
046500040119     C                   movel     '0'           chkcall
046600040119     C                   endif
046700040119     C*
046800040119     C                   ENDSR
046900010601
047000010601
047100010601
047200010330     C*----------------------------------------------------*
047300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047400010330     C*----------------------------------------------------*
047500040119     C     INZVAR        BEGSR
047600010330     C*
047700030822     C* Inizializzo variabili di wrk
047800020204     C                   Z-ADD     *zeros        Num5_0            5 0
047900060331     C                   MOVEL     *blanks       wVATNOT_A
048000060331     C                   MOVEL     *blanks       wVATNOT_B
048100060331     C                   MOVEL     *blanks       wVATNOT_E
048200080925     C                   MOVEL     *blanks       wFlgCAS
048300030822     C*
048400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048500030822     C                   CLEAR                   FIVAB000
048600040119     C                   CLEAR                   FIVAT000
048700030822     C                   CLEAR                   SkSplitCSV
048800030822     C                   EVAL      i = 1
048900030822     C                   EVAL      posDa = *zeros
049000030822     C                   EVAL      posA  = *zeros
049100030822     C*
049200030822     C* Reimposto i valori di default
049300030822     C                   EXSR      DEFCAM
049400030822     C*
049500010330     C                   ENDSR
049600010330     C*----------------------------------------------------*
049700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049800010330     C*----------------------------------------------------*
049900010330     C     DEFCAM        BEGSR
050000030822     C*
050100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050200030715     C* e delimitatore testo.
050300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050600030822     C*
050700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
050900030715     C                   SETON                                        50
051000030715     C                   ELSE
051100030715     C                   SETOFF                                       50
051200030715     C                   ENDIF
051300030822     C*
051400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051500030822     C                   EVAL      posDaDft = 1
051600030822     C                   EVAL      posADft  = 0
051700030822     C                   EVAL      wGiroDft = 0
051800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051900030822     C                             posDaDft > 0
052000030822     C*
052100030822     C* Gestisco il 1° giro
052200030822     C                   IF        wGiroDft = 0
052300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052500030822     C* Incremento il contatore dei "giri"
052600030822     C                   EVAL      wGiroDft = 1
052700030822     C                   ELSE
052800030822     C                   EVAL      posDaDft = posADft
052900030822     C                   ENDIF
053000030822     C* Eseguo lo scan x trovare la fine del campo corrente
053100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053200030822     C*
053300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053500030822     C                   IF        posDaDft > 0 AND
053600030822     C                             posADft  > 0
053700030822     C* NCL
053800030822     C                   IF        %subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):1:3)
054100030822     C                             = 'NCL'
054200030822     C                   EVAL      PiStr=%trim(%subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500030822     C                   EXSR      CHKNUM
054600030822     C                   IF        PiInt=*on
054700030822     C                   Z-ADD     PiVal         VABNCL
054800030822     C                   ENDIF
054900030822     C                   ENDIF
055000030822     C* CCM
055100030822     C                   IF        %subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):1:3)
055400030822     C                             = 'CCM'
055500030822     C                   EVAL      PiStr=%trim(%subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055800030822     C                   EXSR      CHKNUM
055900030822     C                   IF        PiInt=*on
056000030822     C                   Z-ADD     PiVal         VABCCM
056100030822     C                   ENDIF
056200030822     C                   ENDIF
056300030822     C* LNP
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'LNP'
056800030822     C                   EVAL      PiStr=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   EXSR      CHKNUM
057200030822     C                   IF        PiInt=*on
057300030822     C                   Z-ADD     PiVal         VABLNP
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* NRS
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'NRS'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABNRS
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* CTR
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'CTR'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABCTR
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* PKB
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'PKB'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiNum=*on
061200030822     C                   Z-ADD     PiVal         VABPKB
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* VLB
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'VLB'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiNum=*on
062500030822     C                   Z-ADD     PiVal         VABVLB
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* QFT
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'QFT'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiNum=*on
063800030822     C                   Z-ADD     PiVal         VABQFT
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* CBO
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'CBO'
064600030822     C                   EVAL      VABCBO=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   ENDIF
065000030822     C* TSP
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'TSP'
065500030822     C                   EVAL      VABTSP=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* VAS
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'VAS'
066400030822     C                   EVAL      VABVAS=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* VCA
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'VCA'
067300030822     C                   EVAL      VABVCA=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700070925     C* TIC
067800070925     C                   IF        %subst(
067900070925     C                             %subst(vlrppt:posDaDft+1:
068000070925     C                             posADft-posDaDft-1):1:3)
068100070925     C                             = 'TIC'
068200070925     C                   EVAL      VABTIC=%trim(%subst(
068300070925     C                             %subst(vlrppt:posDaDft+1:
068400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500070925     C                   ENDIF
068600030822     C* GCA
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'GCA'
069100030822     C                   EVAL      VABGCA=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500030822     C* CTM
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'CTM'
070000030822     C                   EVAL      VABCTM=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* FFD
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'FFD'
070900030822     C                   EVAL      VABFFD=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* VAD
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'VAD'
071800030822     C                   EVAL      VABVAD=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* GMA
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'GMA'
072700030822     C                   EVAL      VABGMA=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* GGA
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'GGA'
073600030822     C                   EVAL      VABGGA=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* GVA
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'GVA'
074500030822     C                   EVAL      VABGVA=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* TC1
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'TC1'
075400030822     C                   EVAL      VABTC1=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* TC2
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'TC2'
076300030822     C                   EVAL      VABTC2=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700040714     C* VATTRC
076800040714     C                   IF        %subst(
076900040714     C                             %subst(vlrppt:posDaDft+1:
077000040714     C                             posADft-posDaDft-1):1:3)
077100040714     C                             = 'TRC'
077200040714     C                   EVAL      VATTRC=%trim(%subst(
077300040714     C                             %subst(vlrppt:posDaDft+1:
077400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500040714     C                   ENDIF
077600030822     C* ...
077700030822     C                   ENDIF
077800030822     C                   ENDDO
077900020204     C*
078000010330     C                   ENDSR
078100010607     C*----------------------------------------------------*
078200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078300010607     C*----------------------------------------------------*
078400010607     C     IMPVAB        BEGSR
078500010607     C*
078600010607     C                   SETOFF                                       3132
078700030515     C*
078800010607     C                   MOVEL     datcor        VABAAS
078900010607     C                   MOVE      datcor        VABMGS
079000010607     C*
079100030715     C**********
079200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079300030715     C**********
079400030715     C                   IF        wGiro = *zeros
079500040113     C*
079600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079800040510     C                             %trim(intcol(3))<>*blanks
079900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080000040113     C                                      %trim(intcol(3))
080100070823     C     ';':CharCSV   XLATE     vindta        vindta
080200040510     C                   ENDIF
080300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080400031201     C     minu:maiu     XLATE     vindta        vindta
080500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080600030715     C                   DOW       posDa <= %len(%trim(vindta))
080700030715     C*
080800030715     C* Gestisco il 1° campo
080900030715     C                   IF        i = 1
081000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081100030715     C                   EVAL      posDa = 1
081200030715     C* Eseguo lo scan x trovare la fine del primo campo
081300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081400030715     C* A questo punto "estraggo" il campo corrente
081500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081600030715     C                                                    (posA-posDa))
081700030715     C* X i campi successivi al 1°
081800030715     C                   ELSE
081900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082000030715     C                   EVAL      posDa = posA + 1
082100030715     C* Eseguo lo scan x trovare la fine del campo corrente
082200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082300030715     C* Gestisco l'ultimo campo
082400030715     C                   IF        posA = *zeros
082500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082600030715     C* A questo punto "estraggo" il campo corrente
082700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082800030715     C                                                    (posA-posDa))
082900030715     C                   LEAVE
083000030715     C                   ELSE
083100030715     C* A questo punto "estraggo" il campo corrente
083200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083300030715     C                                                    (posA-posDa))
083400030715     C                   ENDIF
083500030715     C                   ENDIF
083600030715     C* Incremento il contatore d campo
083700030715     C                   EVAL      i = i +1
083800030715     C                   ENDDO
083900030715     C*
084000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084100030715     C* delimitazione testo
084200030715     C                   EVAL      i = 1
084300030715     C                   DOW       i < %elem(SkSplitFLD)
084400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084600030715     C                   EVAL      i = i + 1
084700030715     C                   ENDDO
084800030715     C                   EVAL      wGiro = 1
084900030715     C                   ELSE
085000030715     C                   EVAL      wGiro = 2
085100031204     C**********
085200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085300031204     C* d separatore campo
085400031204     C**********
085500031204     C                   EVAL      vindta = %trim(vindta)
085600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085700031204     C                   EVAL      vindta = %subst(vindta:2)
085800031204     C                   ENDDO
085900031204     C*
086000031204     C                   Z-ADD     *zeros        lunghInput        4 0
086100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086500031204     C                   ENDDO
086600030715     C**********
086700030715     C* Effettuo lo split del campo dati d input
086800030715     C**********
086900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087000030715     C                   DOW       posDa <= %len(%trim(vindta))
087100030715     C*
087200030715     C* Gestisco il 1° campo
087300030715     C                   IF        i = 1
087400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087500030715     C                   EVAL      posDa = 1
087600030715     C* Eseguo lo scan x trovare la fine del primo campo
087700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087800030715     C* A questo punto "estraggo" il campo corrente
087900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088000030715     C                                                    (posA-posDa))
088100030715     C* X i campi successivi al 1°
088200030715     C                   ELSE
088300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088400030715     C                   EVAL      posDa = posA + 1
088500031204     C* Verifico che nn vi sia il campo nn valorizzato
088600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088700031204     C* Se campo nn valorizzato skippo al prossimo
088800031204     C                   EVAL      SkSplitCSV(i) = *blanks
088900031204     C                   EVAL      posA  = posA + 1
089000031204     C                   ELSE
089100030715     C* Eseguo lo scan x trovare la fine del campo corrente
089200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089300030715     C* Gestisco l'ultimo campo
089400030715     C                   IF        posA = *zeros
089500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C                   LEAVE
090000030715     C                   ELSE
090100030715     C* A questo punto "estraggo" il campo corrente
090200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090300030715     C                                                    (posA-posDa))
090400030715     C                   ENDIF
090500031204     C                   ENDIF
090600030715     C                   ENDIF
090700030715     C* Incremento il contatore d campo
090800030715     C                   EVAL      i = i +1
090900030715     C                   ENDDO
091000030715     C*
091100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091200030715     C* delimitazione testo
091300030715     C                   EVAL      i = 1
091400030715     C                   DOW       i < %elem(SkSplitCSV)
091500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091700030715     C                   EVAL      i = i + 1
091800030715     C                   ENDDO
091900030715     C*
092000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092100030715     C                   EVAL      i = 1
092200030715     C                   DOW       i < %elem(SkSplitFLD)
092300030715     C*
092400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092600030715     C                             SkSplitCSV(i) <> *blanks
092700030721     C*** ATB
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092900030721     C                   EVAL      VABATB = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030715     C*** RSD
093200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093600030715     C* ==
093700030715     C                   ENDIF
093800030721     C*** RD2
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030715     C*** IND
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094400030715     C                   EVAL      VABIND = SkSplitCSV(i)
094500030715     C                   ENDIF
094600030715     C*** LOD
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
094800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** PRD
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
095200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030721     C*** NZD
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
095600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** RMA
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** NOT
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** NT2
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
096800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** VCA
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
097200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** VAS
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
097600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** FFD
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030721     C*** GC1
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** GC2
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** TSP
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
099200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099300030721     C                   ENDIF
099400040510     C*** CBO
099500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099700040510     C                   ENDIF
099800030721     C*** NAS
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** TIC
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** GCA
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** XCO
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** CTM
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TCR
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** CTS
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** FTM
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** VAD
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** GMA
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** GGA
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GVA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** TC1
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** TC2
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** SCL
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** RMO
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** NMO
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*
106700030721     C* Reperisco quindi i campi numerici...
106800040510     C*** NSP
106900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107000040510     C                   EVAL      PiStr=SkSplitCSV(i)
107100040510     C                   EXSR      CHKNUM
107200040510     C                   IF        PiInt=*on
107300040510     C                   Z-ADD     PiVal         VABNSP
107400040510     C                   ELSE
107500040510     C                   SETON                                        31
107600040510     C                   EVAL      VABNSP = *zeros
107700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107800040510     C                             + ' ' + 'VABNSP'
107900040510     C                   ENDIF
108000040510     C                   ENDIF
108100071106     C*** RMN
108200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108300071106     C                   EVAL      PiStr=SkSplitCSV(i)
108400071106     C                   EXSR      CHKNUM
108500071106     C                   IF        PiInt=*on
108600071106     C                   Z-ADD     PiVal         VABRMN
108700071106     C                   ELSE
108800071106     C                   SETON                                        32
108900071106     C                   EVAL      VABRMN = *zeros
109000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109100071106     C                             + ' ' + 'VABRMN'
109200071106     C                   ENDIF
109300071106     C                   ENDIF
109400030721     C*** CAD
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109600030721     C                   EVAL      PiStr=SkSplitCSV(i)
109700030721     C                   EXSR      CHKNUM
109800030721     C                   IF        PiInt=*on
109900030721     C                   Z-ADD     PiVal         Num5_0
110000030721     C                   MOVEL(p)  Num5_0        VABCAD
110100030721     C                   ELSE
110200030721     C                   SETON                                        32
110300030721     C                   EVAL      VABCAD = *zeros
110400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110500030721     C                             + ' ' + 'VABCAD'
110600030721     C                   ENDIF
110700030721     C                   ENDIF
110800030721     C*** NCL
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiInt=*on
111300030721     C                   Z-ADD     PiVal         VABNCL
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABNCL = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800040122     C                             + ' ' + 'VABNCL'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** PKB
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiNum=*on
112600030721     C                   Z-ADD     PiVal         VABPKB
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABPKB = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100030721     C                             + ' ' + 'VABPKB'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** CAS
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113600080125     C                   IF        SkSplitCSV(i) <> *blanks
113700080125     C                   EVAL      wFlgCAS = '1'
113800080125     C                   ENDIF
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiNum=*on
114200030721     C                   Z-ADD     PiVal         VABCAS
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABCAS = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABCAS'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** CCM
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on      AND
115500030721     C                             PiVal<=9999999 AND
115600030721     C                             PiVal>*zeros
115700030721     C                   Z-ADD     PiVal         VABCCM
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000030721     C                   EVAL      VABCCM = *zeros
116100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116200040510     C                             + ' ' + 'VABCCM'
116300030721     C                   ENDIF
116400030721     C                   ENDIF
116500030721     C*** LNP
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiInt=*on    AND
117000030721     C                             PiVal<=999   AND
117100030721     C                             PiVal>*zeros
117200030721     C                   Z-ADD     PiVal         VABLNP
117300030721     C                   ELSE
117400040715     C                   SETON                                        32
117500030721     C                   EVAL      VABLNP = *zeros
117600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117700040510     C                             + ' ' + 'VABLNP'
117800030721     C                   ENDIF
117900030721     C                   ENDIF
118000030721     C*** NRS
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiInt=*on AND
118500030721     C                             PiVal<=99
118600030721     C                   Z-ADD     PiVal         VABNRS
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABNRS'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** LNA
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on    AND
119800030721     C                             PiVal<=999   AND
119900030721     C                             PiVal>*zeros
120000030721     C                   Z-ADD     PiVal         VABLNA
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      VABLNA = *zeros
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABLNA'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** CTR
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiInt=*on  AND
121300030721     C                             PiVal<=999
121400030721     C                   Z-ADD     PiVal         VABCTR
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABCTR'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** IAS
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiNum=*on
122600030721     C                   Z-ADD     PiVal         VABIAS
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABIAS'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** VLB
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiNum=*on
123800030721     C                   Z-ADD     PiVal         VABVLB
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABVLB'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** QFT
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiNum=*on
125000030721     C                   Z-ADD     PiVal         VABQFT
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABQFT'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** NCD
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on      AND
126200030721     C                             PiVal<=9999999
126300030721     C                   Z-ADD     PiVal         VABNCD
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABNCD'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** NCA
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on      AND
127500030721     C                             PiVal<=9999999
127600030721     C                   Z-ADD     PiVal         VABNCA
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABNCA'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** ZNC
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on AND
128800030721     C                             PiVal<=99
128900030721     C                   Z-ADD     PiVal         VABZNC
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABZNC'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** DCR
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on       AND
130100030721     C                             PiVal<=99999999
130200030721     C                   Z-ADD     PiVal         VABDCR
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABDCR'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** HCR
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on   AND
131400030721     C                             PiVal<=9999
131500030721     C                   Z-ADD     PiVal         VABHCR
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABHCR'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** VMD
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiNum=*on
132700030721     C                   Z-ADD     PiVal         VABVMD
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABVMD'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** ANT
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on        AND
133900030721     C                             PiVal<=999999999
134000030721     C                   Z-ADD     PiVal         VABANT
134100030721     C                   ELSE
134200030721     C                   SETON                                        32
134300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134400030721     C                             + ' ' + 'VABANT'
134500030721     C                   ENDIF
134600030721     C                   ENDIF
134700030721     C*** CMO
134800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
134900030721     C                   EVAL      PiStr=SkSplitCSV(i)
135000030721     C                   EXSR      CHKNUM
135100030721     C                   IF        PiInt=*on
135200030721     C                   Z-ADD     PiVal         Num5_0
135300030721     C                   MOVEL(p)  Num5_0        VABCMO
135400030721     C                   ELSE
135500030721     C                   SETON                                        32
135600030721     C                   EVAL      VABCMO = *zeros
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABCMO'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100040510     C*
136200040510     C* Reperisco i campi del FIVAT
136300040510     C*** VATNOT
136400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
136500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
136600040510     C                   ENDIF
136700040510     C*** VATTRC
136800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
136900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137000040510     C                   ENDIF
137100060331     C*** VATNOT_A
137200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137400060331     C                   ENDIF
137500060331     C*** VATNOT_B
137600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
137700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
137800071106     C                   EVAL      VABNOT    = %trim(SkSplitCSV(i))
137900060331     C                   ENDIF
138000060331     C*** VATNOT_E
138100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138300060331     C                   ENDIF
138400070823     C*
138500071029     C***  ===>  Gestione campi "particolari" dentro ciclo
138600071204     C*
138700071204     C*** NSP/RMN
138800071204     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
138900071204     C                   EVAL      PiStr=SkSplitCSV(i)
139000071204     C                   EXSR      CHKNUM
139100071204     C                   IF        PiInt=*on
139200071204     C                   Z-ADD     PiVal         VABNSP
139300071204     C                   Z-ADD     PiVal         VATNSP
139400071204     C                   Z-ADD     PiVal         VABRMN
139500071204     C                   ELSE
139600071204     C                   SETON                                        32
139700071204     C                   EVAL      VABNSP = *zeros
139800071204     C                   EVAL      VATNSP = *zeros
139900071204     C                   EVAL      VABRMN = *zeros
140000071204     C                   EVAL      vinmsg = %trimr(vinmsg)
140100071204     C                             + ' ' + 'VABNSP VATNSP VABRMN'
140200071204     C                   ENDIF
140300071204     C                   ENDIF
140400071204     C*
140500071204     C*** PKB
140600071204     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
140700071204     C                   EVAL      PiStr=SkSplitCSV(i)
140800071204     C                   EXSR      CHKNUM
140900071204     C                   IF        PiNum=*on
141000071204     C                   Z-ADD(H)  PiVal         VABPKB
141100071204     C                   ELSE
141200071204     C                   SETON                                        32
141300071204     C                   EVAL      VABPKB = *zeros
141400071204     C                   EVAL      vinmsg = %trimr(vinmsg)
141500071204     C                             + ' ' + 'VABPKB'
141600071204     C                   ENDIF
141700071204     C                   ENDIF
141800070925     C*
141900071029     C***  <===  -----------------------------------------
142000040510     C*
142100030715     C                   ENDIF
142200030715     C                   EVAL      i = i + 1
142300030715     C                   ENDDO
142400070925     C*
142500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142600080125     C*
142700080125     C* CTR / CTM
142800080125     C                   IF        VABCTR = 353
142900080125     C                   EVAL      VABCTM = '7R'
143000080125     C                   ENDIF
143100070925     C*
143200071029     C***  <===  -----------------------------------------
143300070925     C*
143400070925     C* Considerazioni finali su CBO/CAS
143500080125     C                   IF        wFlgCAS = '1'
143600070925     C                   IF        VABCBO = '1'
143700070925     C                   EVAL      VABCBO = '4'
143800070925     C                   ENDIF
143900070925     C                   IF        VABCBO = '2'
144000070925     C                   EVAL      VABCBO = '6'
144100070925     C                   ENDIF
144200070925     C                   ENDIF
144300070925     C***  <===  ----------------------------
144400040510     C*
144500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
144600040510     C                   IF        VABNSP = *zeros
144700040510     C                   SETON                                        31
144800040510     C                   ENDIF
144900040715     C*
145000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
145100040715     C                   IF        VABLNP = *zeros
145200040715     C                   SETON                                        31
145300040715     C                   ENDIF
145400040510     C*
145500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
145600040510     C                   IF        VABRMN = *zeros
145700040510     C                   Z-ADD     VABNSP        VABRMN
145800040510     C                   ENDIF
145900040510     C*
146000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
146100040510     C                   IF        VABRMA = *blanks
146200040714     C                   MOVEL     VABRMN        VABRMA
146300040510     C                   ENDIF
146400040128     C*
146500040510     C* Se provincia nn valorizzata la reperisco
146600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146700040510     C                   IF        VABPRD = *blanks
146800040128     C                   CLEAR                   TISI95DS
146900040128     C                   EVAL      I95TCN = '3'
147000040128     C                   Z-ADD     datcor        I95DAT
147100040128     C                   EVAL      I95NAR = VABNZD
147200040128     C                   EVAL      I95CAP = VABCAD
147300040128     C                   EVAL      I95LOC = VABLOD
147400040128     C                   CALL      'TISI95R'
147500040128     C                   PARM                    TISI95DS
147600040128     C                   EVAL      VABPRD = O95PRV
147700040510     C                   ENDIF
147800040113     C*
147900040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
148000040113     C                   IF        VABCAS > *zeros
148100040119     C                   EVAL      VABVCA = 'EUR'
148200040119     C                   IF        VABCBO = '1'
148300040113     C                   EVAL      VABCBO = '4'
148400040119     C                   ENDIF
148500040119     C                   IF        VABCBO = '2'
148600040119     C                   EVAL      VABCBO = '6'
148700040119     C                   ENDIF
148800040113     C                   ENDIF
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
150600020204     C* Come prima cosa effettuo considerazioni sulla divisa
150700020204     C                   IF        vabIAS > *zeros
150800020204     C                   IF        vabVAS <> 'EUR'
150900020204     C                   EVAL      vabVAS =  'ITL'
151000020204     C                   ENDIF
151100020204     C                   ENDIF
151200020204     C*
151300020204     C                   IF        vabCAS > *zeros
151400020204     C                   IF        vabVCA <> 'EUR'
151500020204     C                   EVAL      vabVCA =  'ITL'
151600020204     C                   ENDIF
151700020204     C                   ENDIF
151800020204     C*
151900020204     C                   IF        vabVMD > *zeros
152000020204     C                   IF        vabVAD <> 'EUR'
152100020204     C                   EVAL      vabVAD =  'ITL'
152200020204     C                   ENDIF
152300020204     C                   ENDIF
152400020204     C*
152500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152600020204     C                   Z-ADD     vabIAS        wrkDec
152700020204     C                   IF        wrkDec > *zeros
152800020204     C                   IF        vabVAS = 'ITL'
152900020204     C                   EVAL      vabIAS = *zeros
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153400020204     C                   Z-ADD     vabCAS        wrkDec
153500020204     C                   IF        wrkDec > *zeros
153600020204     C                   IF        vabVCA = 'ITL'
153700020204     C                   EVAL      vabCAS = *zeros
153800020204     C                   ENDIF
153900020204     C                   ENDIF
154000020204     C*
154100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154200020204     C                   Z-ADD     vabVMD        wrkDec
154300020204     C                   IF        wrkDec > *zeros
154400020204     C                   IF        vabVAD = 'ITL'
154500020204     C                   EVAL      vabVMD = *zeros
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C                   ENDSR
155000020204     C***
155100020204
155200010330
155300010330
155400010330     C*----------------------------------------------------*
155500010330     C*  CONTROLLO NUMERICITA' CAMPI
155600010330     C*----------------------------------------------------*
155700010330     C     CHKNUM        BEGSR
155800010330     C*
155900010606     C                   IF        PiDecChr = *blanks
156000030715     C                   EVAL      PiDecChr = CharNUM
156100010606     C                   ENDIF
156200010606     C*
156300010606     C                   CALL(e)   'ISNUMERIC'
156400010330     C                   PARM                    PiStr            30
156500010606     C                   PARM                    PiDecChr          1
156600010330     C                   PARM      *ZEROS        PiVal            30 9
156700010330     C                   PARM      '0'           PiInt             1
156800010330     C                   PARM      '0'           PiNum             1
156900010330     C                   IF        %error
157000010606     C                   EVAL      PiNum=*off
157100010330     C                   ENDIF
157200010330     C*
157300010330     C                   ENDSR
157400010330     C***
157500010330
157600010601
157700010601
157800010601
157900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158000010601     C     repfil        BEGSR
158100010601     C*
158200010601     C                   if        invfil = *zeros and
158300010601     C                             depfil > *zeros and
158400010629     C                             (vinflg = *blanks or
158500010629     C                              vinflg = *zeros)
158600010601     C
158700010601     C                   eval      invfil = depfil
158800010601     C                   endif
158900010601     C*
159000010601     C                   if        depfil <> invfil and
159100010601     C                             invfil > *zeros
159200010601     C                   eval      flgMulti = '1'
159300010601     C                   if        vinflg = *blanks
159400010601     C                   add       1             cntNonEl
159500010601     C                   endif
159600010601     C                   endif
159700010601     C*
159800010601     C                   if        vinflg = '2'
159900010601     C                   eval      flgStato = '2'
160000010601     C                   endif
160100010601     C*
160200010601     C                   ENDSR
160300010601     C***
160400010601
160500010601
160600010601
160700010330
160800040119      /TITLE Invio dei dati al punto operativo.
160900040119     C     invio         BEGSR
161000040119     C*
161100040119     C* 1° invio FIVAT
161200040119     C                   reset                   dscmz
161300040119     C                   move      vlrpoi        cmzdst
161400040119     C                   eval      cmzfld = 'FIVATWWR'
161500040119     C                   eval      cmzmbd = vlrhdl
161600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161700040119     C***                if        prmfir = *blanks
161800040119     C                   eval      cmzfla = 'FIVAT00F'
161900040119     C                   eval      cmzmba = 'FIVAT00F'
162000040119     C***                else
162100040119     C***                eval      cmzfla = prmfir
162200040119     C***                eval      cmzmba = prmfir
162300040119     C***                endif
162400040119     C                   eval      cmznrr = *zeros
162500040119     C                   move      §ctrokvt      cmznrr
162600040119     C                   eval      cmzlba = vlrfl1
162700040119     C                   call(e)   'TIS711C'
162800040119     C                   parm                    dscmz
162900040119     C                   parm      *blanks       esito
163000040119     C                   if        %error
163100040119     C                             or cmzerr = '1'
163200040119     C                             or esito  = '1'
163300040119     C                   eval      wrkesito = '3'
163400040119     C                   else
163500040119     C*
163600040119     C* 2° invio FIVAB
163700040119     C                   reset                   dscmz
163800040119     C                   move      vlrpoi        cmzdst
163900040119     C                   eval      cmzfld = vlrfou
164000040119     C                   eval      cmzmbd = vlrhdl
164100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164200040119     C***                if        prmfir = *blanks
164300040119     C                   eval      cmzfla = 'FIVAB00F'
164400040119     C                   eval      cmzmba = 'FIVAB00F'
164500040119     C***                else
164600040119     C***                eval      cmzfla = prmfir
164700040119     C***                eval      cmzmba = prmfir
164800040119     C***                endif
164900040119     C                   eval      cmznrr = *zeros
165000040119     C                   move      §ctrokvb      cmznrr
165100040119     C                   eval      cmzlba = vlrfl1
165200040119     C                   call(e)   'TIS711C'
165300040119     C                   parm                    dscmz
165400040119     C                   parm      *blanks       esito
165500040119     C                   if        %error
165600040119     C                             or cmzerr = '1'
165700040119     C                             or esito  = '1'
165800040119     C                   eval      wrkesito = '3'
165900040119     C                   endif
166000040119     C                   endif
166100040119     C*
166200040119     C                   ENDSR
166300040119     C***
166400010601
166500010601
166600010601
166700010601
166800010601      /TITLE Invio dei dati al punto operativo.
166900010601     C     opeini        BEGSR
167000010601     C*
167100010601     C* Inizializzo flag e contatori operativi
167200010601     C                   movel     '0'           flgGiro           1
167300010601     C                   movel     '0'           flgMulti          1
167400010601     C                   movel     '1'           flgStato          1
167500010615     C                   movel     '0'           flgOk             1
167600010601     C                   z-add     *zeros        cntNonEl         10 0
167700010601     C                   z-add     *zeros        depfil            3 0
167800010601     C                   z-add     *zeros        invfil            3 0
167900010601     C*
168000010601     C                   ENDSR
168100010601     C***
168200070326
168300070326
168400070326
168500070326
168600070326     C     *pssr         BEGSR
168700070329     C*
168800070329     C                   if        %open(tivin00r)
168900070329     C                   close     tivin00r
169000070329     C                   endif
169100070329     C                   if        %open(fivabwwr)
169200070329     C                   close     fivabwwr
169300070329     C                   endif
169400070329     C                   if        %open(fivatwwr)
169500070329     C                   close     fivatwwr
169600070329     C                   endif
169700070326     C*
169800070326     C* Effettuo la chiamata al CLLE preposto
169900070326     C                   call(e)   'TITVVTC'
170000070326     C                   parm                    parccm
170100070326     C                   parm                    parmbr
170200070326     C                   parm      '2'           paropz
170300070326     C*
170400070326     C                   eval      wrkesito = '2'
170500070404     C*
170600070404     C                   seton                                        LR
170700070326     C*
170800070326     C                   ENDSR     '*CANCL'
170900070326     C***
171000070326
171100070326
171200010330
171300010330
171400000613     C     *inzsr        BEGSR
171500990910     C*
171600990910     C     *entry        plist
171700990920     C                   parm                    tivlrds
171800990921     C                   parm      wrkesito      esito
171900000724     C                   parm                    prmlit
172000000710     C                   parm                    prmfir
172100010330     C*
172200010330     C* CALCOLA LA DATA CORRENTE
172300010330     C                   time                    wn14             14 0
172400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
172500010330     C                   z-add     wn8           g08dat
172600010330     C                   z-add     *zeros        g08inv
172700010330     C                   movel     '0'           g08err
172800010330     C                   call      'XSRDA8'
172900010330     C                   parm                    wlbda8
173000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
173100000613     C*
173200000613     C                   ENDSR
173300000613     C***
173400990908
173500080125     Otitv1d5p  E            riepilogo         2
173600990915     O                                              'Upload via Internet'
173700990915     O                                           +1 'Traduzione TIVIN00R -'
173800021025     O                                           +1 'FIVABWWR'
173900990915     O                                           +1 'Testate bolle'
174000990915     O          E            riepilogo   2
174100990915     O                       wrkdata
174200990915     O                       wrkora              +1
174300990915     O                       procname            +1
174400990915     O          E            riepilogo   2
174500990915     O                                              'Cliente..................:'
174600010605     O                       VABCCM        z     +1
174700990915     O          E            riepilogo   2
174800990920     O                                              'Riferimento Strategi.....:'
174900990920     O                       vlrhdl              +1
175000990915     O          E            riepilogo   2
175100990915     O                                              'Giusti...................:'
175200040119     O                       §CTROKVB      2   +  1
175300990915     O          E            riepilogo   2
175400990915     O                                              'Sbagliati e corretti.....:'
175500971022     O                       §CTRMO        2   +  1
175600990915     O          E            riepilogo   2
175700990915     O                                              'Sbagliati e scartati.....:'
175800971022     O                       §CTRNO        2   +  1
175900000613
176000080125     Otitv1d5ps E            testdett          2
176100000613     O                                              'Upload via Internet'
176200000613     O                                           +1 'Traduzione TIVIN00R -'
176300021025     O                                           +1 'FIVABWWR'
176400000613     O                                           +1 'Testate bolle'
176500000616     O          E            testdett    3
176600000613     O                                           +2 'N° rec'
176700000613     O                                           +3 'Anteprima contenuto'
176800000616     O          E            rigadett    2
176900000613     O                       rrnum               +2
177000000621     O                       recko               +3
177100000616     O          E            findett     2
177200000613     O                       wrkdata
177300000613     O                       wrkora              +1
177400000613     O                       procname            +1
177500000616     O          E            findett     2
177600000613     O                                              'Cliente..................:'
177700010605     O                       VABCCM        z     +1
177800000616     O          E            findett     2
177900000613     O                                              'Riferimento Strategi.....:'
178000000613     O                       vlrhdl              +1
178100000616     O          E            findett     2
178200000613     O                                              'Giusti...................:'
178300040119     O                       §CTROKVB      2   +  1
178400000616     O          E            findett     2
178500000613     O                                              'Sbagliati e corretti.....:'
178600000613     O                       §CTRMO        2   +  1
178700000616     O          E            findett     2
178800000613     O                                              'Sbagliati e scartati.....:'
178900000613     O                       §CTRNO        2   +  1
179000000613** CMD - COMANDI CL
179100080125OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1D5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
179200080125OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1D5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
179300080125DLTOVR FILE(TITV1D5P TITV1D5PS) LVL(*)
179400010606
179500000613
179600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179700080317VABNSP;VABCBO;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABNZD;VABCTR;
179800080206VABCAS;VABPKB;FILL02;VABIAS;VATNOT_E;VABNOT;VABNT2;VABRD2
179900080125
