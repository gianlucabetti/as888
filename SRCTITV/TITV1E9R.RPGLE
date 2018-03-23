000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800080325     Ftitv1e9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100080325     Ftitv1e9ps O    f  198        PRINTER usropn
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
013300080325     C                   if        not %open(titv1e9ps)
013400080325     C                   open      titv1e9ps
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
016500080325     C                   if        %open(titv1e9ps)
016600000616     C                   except    findett
016700080325     C                   close     titv1e9ps
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
018700080325     C                   if        not %open(titv1e9p)
018800080325     C   50              open      titv1e9p
018900990915     C                   endif
019000990915     C*
019100030715     C   50              except    riepilogo
019200990915     C*
019300080325     C                   if        %open(titv1e9p)
019400080325     C   50              close     titv1e9p
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
023200080325     C*
023300080325     C* Accendo l'indicatore x forzatura scrittura buffers out anche se no num. sped. (vedi Disk A)
023400080325     C                   seton                                        33
023500010330     C*
023600030822     C                   do        *HIVAL
023700010330     C*
023800030822     C                   read      tivin00r                               70
023900010618     C*
024000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
024100010618     C                   if        vinflg = '1'
024200010618     C                   eval      flgOk = '1'
024300010618     C                   endif
024400040510     C*
024500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024600040510     C                   z-add     *zeros        wLenVINDTA        4 0
024700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
025000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
025100040510     C                   z-add     1             wFlgVINDTA
025200040510     C                   leave
025300040510     C                   endif
025400040510     C                   enddo
025500010618     C*
025600040510     C                   if        vindta > *blanks AND
025700040510     C                             wFlgVINDTA = 1
025800010330     C                   add       1             rrnum
025900010330     C*
026000010601     C                   if        *in70 = *off and
026100010330     C                             (vinflg = *blanks
026200010330     C                              or vinflg = '0'
026300010330     C                              or vinflg = '2')
026400010330     C*
026500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026600010711     C                   if        vinflg = *blanks or vinflg = '0'
026700010711     C                   clear                   vinmsg
026800010711     C                   endif
026900010601     C*
027000070925     C* Aggiungo un byte a blanks all'inizio d ogni record
027100070925     C                   eval      vindta  = ' ' + %trim(vindta)
027200070925     C*
027300010330     C                   exsr      inzvar
027400010605     C                   exsr      impvab
027500010601     C*
027600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027700010605     C                   eval      depfil = VABLNP
027800010601     C                   exsr      repfil
027900010601     C                   if        depfil = invfil
028000021025     C                   if        vlrpoi = 999
028100030822     C                   move(P)   invfil        VABFGS
028200021025     C                   else
028300030822     C                   move(P)   vlrpoi        VABFGS
028400021025     C                   endif
028500010601     C*
028600010601     C                   exsr      PREELA
028700010601     C*
028800010604     C* Ebbene...
028900010604     C*
029000040119     C  N31              add       1             §CTROKVB
029100040119     C   32              add       1             §CTRMO
029200040119     C   31              add       1             §CTRNO
029300030822     C                   if        wGiro = 2
029400040510     C* Scrivo sempre il VAT
029500040510     C  N31              exsr      wrivat
029600040510     C* Scrivo il VAB solo a rottura d spedizione
029700040119     C                   if        depspe =  *zeros OR
029800040119     C                             depspe <> vabNSP
029900040119     C  N31              exsr      wrivab
030000040119     C  N31              eval      depspe = vabNSP
030100040119     C                   endif
030200040119     C                   endif
030300010604     C*
030400010604     C                   if        *in31 = *off and
030500010604     C                             *in32 = *off
030600010604     C                   eval      vinflg = '1'
030700010604     C                   else
030800010604     C                   eval      recko = vindta
030900010604     C                   except    rigadett
031000010604     C                   eval      vinflg = '2'
031100010604     C                   endif
031200010604     C                   endif
031300010601     C*
031400010604     C                   endif
031500010604     C*
031600010330     C                   else
031700010330     C                   eval      vinflg = '1'
031800010330     C                   endif
031900010601     C*
032000010601     C  N70              update    tivin000
032100010330     C*
032200030822     C  N70              enddo
032300040119     C*
032400040119     C                   endif                                                  (endif - chkcall)
032500010601     C*
032600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
032700010601     C                   if        cntNonEl = *zeros or
032800010601     C                             flgMulti = '0'
032900010330     C* Se non ci sono record con errori ...
033000010601     C                   if        §ctrno = 0 and
033100010604     C                             §ctrmo = 0 and
033200010601     C                             flgStato <> '2'
033300010330     C* ... restituisco esito OK.
033400010330     C                   eval      wrkesito = '0'
033500010330     C                   else
033600040119     C                   if        §ctrokvb > 0
033700010330     C                   eval      wrkesito = '1'
033800010330     C                   else
033900010615     C                   if        flgOk = '0'
034000010615     C                   eval      wrkesito = '2'
034100010615     C                   else
034200010615     C                   eval      wrkesito = '6'
034300010615     C                   endif
034400010330     C                   endif
034500010330     C                   endif
034600010601     C                   else
034700010601     C                   eval      wrkesito = '9'
034800010601     C                   endif
034900010330     C*
035000010330     C                   if        %open(tivin00r)
035100010330     C                   close     tivin00r
035200010330     C                   endif
035300021025     C                   if        %open(fivabwwr)
035400021025     C                   close     fivabwwr
035500010330     C                   endif
035600040512     C                   if        %open(fivatwwr)
035700040512     C                   close     fivatwwr
035800040512     C                   endif
035900010601     C*
036000010601     C                   if        vlrpoi <> 999
036100010601     C                   eval      invfil = vlrpoi
036200010601     C                   endif
036300010330     C*
036400040119     C                   if        §ctrokvb > 0
036500010601     C                             and invfil > *zeros
036600010330     C                   exsr      invio
036700010330     C                   endif
036800010601     C*
036900010618     C                   if        flgGiro = '1'
037000010601     C                   exsr      endela
037100010618     C                   endif
037200010330     C*
037300010330     C                   ENDSR
037400010330     C***
037500040119
037600040119
037700040119
037800040119     C*----------------------------------------------------*
037900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038000040119     C*----------------------------------------------------*
038100040510     C     WRIVAB        BEGSR
038200040119     C*
038300060331     C* Gestisco l'eventuale rottura x numero spedizione
038400080325     C                   if        VABNSP <> depspe OR *in33 = *on
038500070301     C                   if        wVATNOT_E <> *blanks
038600070301     C                   eval      VABCTM = '7Q'
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
048200030822     C*
048300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048400030822     C                   CLEAR                   FIVAB000
048500040119     C                   CLEAR                   FIVAT000
048600030822     C                   CLEAR                   SkSplitCSV
048700030822     C                   EVAL      i = 1
048800030822     C                   EVAL      posDa = *zeros
048900030822     C                   EVAL      posA  = *zeros
049000030822     C*
049100030822     C* Reimposto i valori di default
049200030822     C                   EXSR      DEFCAM
049300030822     C*
049400010330     C                   ENDSR
049500010330     C*----------------------------------------------------*
049600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049700010330     C*----------------------------------------------------*
049800010330     C     DEFCAM        BEGSR
049900030822     C*
050000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050100030715     C* e delimitatore testo.
050200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050500030822     C*
050600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
050800030715     C                   SETON                                        50
050900030715     C                   ELSE
051000030715     C                   SETOFF                                       50
051100030715     C                   ENDIF
051200030822     C*
051300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051400030822     C                   EVAL      posDaDft = 1
051500030822     C                   EVAL      posADft  = 0
051600030822     C                   EVAL      wGiroDft = 0
051700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051800030822     C                             posDaDft > 0
051900030822     C*
052000030822     C* Gestisco il 1° giro
052100030822     C                   IF        wGiroDft = 0
052200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052400030822     C* Incremento il contatore dei "giri"
052500030822     C                   EVAL      wGiroDft = 1
052600030822     C                   ELSE
052700030822     C                   EVAL      posDaDft = posADft
052800030822     C                   ENDIF
052900030822     C* Eseguo lo scan x trovare la fine del campo corrente
053000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053100030822     C*
053200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053400030822     C                   IF        posDaDft > 0 AND
053500030822     C                             posADft  > 0
053600030822     C* NCL
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'NCL'
054100030822     C                   EVAL      PiStr=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   EXSR      CHKNUM
054500030822     C                   IF        PiInt=*on
054600030822     C                   Z-ADD     PiVal         VABNCL
054700030822     C                   ENDIF
054800030822     C                   ENDIF
054900030822     C* CCM
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'CCM'
055400030822     C                   EVAL      PiStr=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   EXSR      CHKNUM
055800030822     C                   IF        PiInt=*on
055900030822     C                   Z-ADD     PiVal         VABCCM
056000030822     C                   ENDIF
056100030822     C                   ENDIF
056200030822     C* LNP
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'LNP'
056700030822     C                   EVAL      PiStr=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   EXSR      CHKNUM
057100030822     C                   IF        PiInt=*on
057200030822     C                   Z-ADD     PiVal         VABLNP
057300030822     C                   ENDIF
057400030822     C                   ENDIF
057500030822     C* NRS
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'NRS'
058000030822     C                   EVAL      PiStr=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   EXSR      CHKNUM
058400030822     C                   IF        PiInt=*on
058500030822     C                   Z-ADD     PiVal         VABNRS
058600030822     C                   ENDIF
058700030822     C                   ENDIF
058800030822     C* CTR
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'CTR'
059300030822     C                   EVAL      PiStr=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   EXSR      CHKNUM
059700030822     C                   IF        PiInt=*on
059800030822     C                   Z-ADD     PiVal         VABCTR
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* PKB
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'PKB'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiNum=*on
061100030822     C                   Z-ADD     PiVal         VABPKB
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* VLB
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'VLB'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiNum=*on
062400030822     C                   Z-ADD     PiVal         VABVLB
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* QFT
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'QFT'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiNum=*on
063700030822     C                   Z-ADD     PiVal         VABQFT
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* CBO
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'CBO'
064500030822     C                   EVAL      VABCBO=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   ENDIF
064900030822     C* TSP
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'TSP'
065400030822     C                   EVAL      VABTSP=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* VAS
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'VAS'
066300030822     C                   EVAL      VABVAS=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* VCA
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VCA'
067200030822     C                   EVAL      VABVCA=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600070925     C* TIC
067700070925     C                   IF        %subst(
067800070925     C                             %subst(vlrppt:posDaDft+1:
067900070925     C                             posADft-posDaDft-1):1:3)
068000070925     C                             = 'TIC'
068100070925     C                   EVAL      VABTIC=%trim(%subst(
068200070925     C                             %subst(vlrppt:posDaDft+1:
068300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400070925     C                   ENDIF
068500030822     C* GCA
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'GCA'
069000030822     C                   EVAL      VABGCA=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* CTM
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'CTM'
069900030822     C                   EVAL      VABCTM=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* FFD
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'FFD'
070800030822     C                   EVAL      VABFFD=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* VAD
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'VAD'
071700030822     C                   EVAL      VABVAD=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* GMA
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'GMA'
072600030822     C                   EVAL      VABGMA=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* GGA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'GGA'
073500030822     C                   EVAL      VABGGA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GVA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GVA'
074400030822     C                   EVAL      VABGVA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* TC1
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'TC1'
075300030822     C                   EVAL      VABTC1=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* TC2
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'TC2'
076200030822     C                   EVAL      VABTC2=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600040714     C* VATTRC
076700040714     C                   IF        %subst(
076800040714     C                             %subst(vlrppt:posDaDft+1:
076900040714     C                             posADft-posDaDft-1):1:3)
077000040714     C                             = 'TRC'
077100040714     C                   EVAL      VATTRC=%trim(%subst(
077200040714     C                             %subst(vlrppt:posDaDft+1:
077300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400040714     C                   ENDIF
077500030822     C* ...
077600030822     C                   ENDIF
077700030822     C                   ENDDO
077800020204     C*
077900010330     C                   ENDSR
078000010607     C*----------------------------------------------------*
078100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078200010607     C*----------------------------------------------------*
078300010607     C     IMPVAB        BEGSR
078400010607     C*
078500010607     C                   SETOFF                                       3132
078600030515     C*
078700010607     C                   MOVEL     datcor        VABAAS
078800010607     C                   MOVE      datcor        VABMGS
078900010607     C*
079000030715     C**********
079100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079200030715     C**********
079300030715     C                   IF        wGiro = *zeros
079400040113     C*
079500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079700040510     C                             %trim(intcol(3))<>*blanks
079800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079900040113     C                                      %trim(intcol(3))
080000070823     C     ';':CharCSV   XLATE     vindta        vindta
080100040510     C                   ENDIF
080200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080300031201     C     minu:maiu     XLATE     vindta        vindta
080400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080500030715     C                   DOW       posDa <= %len(%trim(vindta))
080600030715     C*
080700030715     C* Gestisco il 1° campo
080800030715     C                   IF        i = 1
080900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081000030715     C                   EVAL      posDa = 1
081100030715     C* Eseguo lo scan x trovare la fine del primo campo
081200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081300030715     C* A questo punto "estraggo" il campo corrente
081400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081500030715     C                                                    (posA-posDa))
081600030715     C* X i campi successivi al 1°
081700030715     C                   ELSE
081800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081900030715     C                   EVAL      posDa = posA + 1
082000030715     C* Eseguo lo scan x trovare la fine del campo corrente
082100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082200030715     C* Gestisco l'ultimo campo
082300030715     C                   IF        posA = *zeros
082400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082500030715     C* A questo punto "estraggo" il campo corrente
082600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082700030715     C                                                    (posA-posDa))
082800030715     C                   LEAVE
082900030715     C                   ELSE
083000030715     C* A questo punto "estraggo" il campo corrente
083100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083200030715     C                                                    (posA-posDa))
083300030715     C                   ENDIF
083400030715     C                   ENDIF
083500030715     C* Incremento il contatore d campo
083600030715     C                   EVAL      i = i +1
083700030715     C                   ENDDO
083800030715     C*
083900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084000030715     C* delimitazione testo
084100030715     C                   EVAL      i = 1
084200030715     C                   DOW       i < %elem(SkSplitFLD)
084300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084500030715     C                   EVAL      i = i + 1
084600030715     C                   ENDDO
084700030715     C                   EVAL      wGiro = 1
084800030715     C                   ELSE
084900030715     C                   EVAL      wGiro = 2
085000031204     C**********
085100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085200031204     C* d separatore campo
085300031204     C**********
085400031204     C                   EVAL      vindta = %trim(vindta)
085500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085600031204     C                   EVAL      vindta = %subst(vindta:2)
085700031204     C                   ENDDO
085800031204     C*
085900031204     C                   Z-ADD     *zeros        lunghInput        4 0
086000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086400031204     C                   ENDDO
086500030715     C**********
086600030715     C* Effettuo lo split del campo dati d input
086700030715     C**********
086800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086900030715     C                   DOW       posDa <= %len(%trim(vindta))
087000030715     C*
087100030715     C* Gestisco il 1° campo
087200030715     C                   IF        i = 1
087300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087400030715     C                   EVAL      posDa = 1
087500030715     C* Eseguo lo scan x trovare la fine del primo campo
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C* X i campi successivi al 1°
088100030715     C                   ELSE
088200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088300030715     C                   EVAL      posDa = posA + 1
088400031204     C* Verifico che nn vi sia il campo nn valorizzato
088500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088600031204     C* Se campo nn valorizzato skippo al prossimo
088700031204     C                   EVAL      SkSplitCSV(i) = *blanks
088800031204     C                   EVAL      posA  = posA + 1
088900031204     C                   ELSE
089000030715     C* Eseguo lo scan x trovare la fine del campo corrente
089100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089200030715     C* Gestisco l'ultimo campo
089300030715     C                   IF        posA = *zeros
089400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C                   LEAVE
089900030715     C                   ELSE
090000030715     C* A questo punto "estraggo" il campo corrente
090100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090200030715     C                                                    (posA-posDa))
090300030715     C                   ENDIF
090400031204     C                   ENDIF
090500030715     C                   ENDIF
090600030715     C* Incremento il contatore d campo
090700030715     C                   EVAL      i = i +1
090800030715     C                   ENDDO
090900030715     C*
091000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091100030715     C* delimitazione testo
091200030715     C                   EVAL      i = 1
091300030715     C                   DOW       i < %elem(SkSplitCSV)
091400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091600030715     C                   EVAL      i = i + 1
091700030715     C                   ENDDO
091800030715     C*
091900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092000030715     C                   EVAL      i = 1
092100030715     C                   DOW       i < %elem(SkSplitFLD)
092200030715     C*
092300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092500030715     C                             SkSplitCSV(i) <> *blanks
092600030721     C*** ATB
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092800030721     C                   EVAL      VABATB = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030715     C*** RSD
093100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093500030715     C* ==
093600030715     C                   ENDIF
093700030721     C*** RD2
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
093900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030715     C*** IND
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094300030715     C                   EVAL      VABIND = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** LOD
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
094700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** PRD
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
095100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030721     C*** NZD
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
095500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030715     C*** RMA
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
095900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030715     C*** NOT
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030715     C*** NT2
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
096700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030715     C*** VCA
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
097100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** VAS
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
097500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030715     C*** FFD
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
097900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098000030715     C                   ENDIF
098100030721     C*** GC1
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** GC2
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** TSP
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
099100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099200030721     C                   ENDIF
099300040510     C*** CBO
099400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099600040510     C                   ENDIF
099700030721     C*** NAS
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
099900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** TIC
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** GCA
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** XCO
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** CTM
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** TCR
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
101900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** CTS
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** FTM
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** VAD
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** GMA
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** GGA
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
103900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** GVA
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** TC1
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** TC2
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** SCL
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** RMO
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** NMO
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*
106600030721     C* Reperisco quindi i campi numerici...
106700040510     C*** NSP
106800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
106900040510     C                   EVAL      PiStr=SkSplitCSV(i)
107000040510     C                   EXSR      CHKNUM
107100040510     C                   IF        PiInt=*on
107200040510     C                   Z-ADD     PiVal         VABNSP
107300040510     C                   ELSE
107400040510     C                   SETON                                        31
107500040510     C                   EVAL      VABNSP = *zeros
107600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107700040510     C                             + ' ' + 'VABNSP'
107800040510     C                   ENDIF
107900040510     C                   ENDIF
108000071106     C*** RMN
108100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108200071106     C                   EVAL      PiStr=SkSplitCSV(i)
108300071106     C                   EXSR      CHKNUM
108400071106     C                   IF        PiInt=*on
108500071106     C                   Z-ADD     PiVal         VABRMN
108600071106     C                   ELSE
108700071106     C                   SETON                                        32
108800071106     C                   EVAL      VABRMN = *zeros
108900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109000071106     C                             + ' ' + 'VABRMN'
109100071106     C                   ENDIF
109200071106     C                   ENDIF
109300030721     C*** CAD
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiInt=*on
109800030721     C                   Z-ADD     PiVal         Num5_0
109900030721     C                   MOVEL(p)  Num5_0        VABCAD
110000030721     C                   ELSE
110100030721     C                   SETON                                        32
110200030721     C                   EVAL      VABCAD = *zeros
110300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110400030721     C                             + ' ' + 'VABCAD'
110500030721     C                   ENDIF
110600030721     C                   ENDIF
110700030721     C*** NCL
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on
111200030721     C                   Z-ADD     PiVal         VABNCL
111300030721     C                   ELSE
111400030721     C                   SETON                                        32
111500030721     C                   EVAL      VABNCL = *zeros
111600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111700040122     C                             + ' ' + 'VABNCL'
111800030721     C                   ENDIF
111900030721     C                   ENDIF
112000030721     C*** PKB
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112200030721     C                   EVAL      PiStr=SkSplitCSV(i)
112300030721     C                   EXSR      CHKNUM
112400030721     C                   IF        PiNum=*on
112500030721     C                   Z-ADD     PiVal         VABPKB
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      VABPKB = *zeros
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000030721     C                             + ' ' + 'VABPKB'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** CAS
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiNum=*on
113800030721     C                   Z-ADD     PiVal         VABCAS
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      VABCAS = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300030721     C                             + ' ' + 'VABCAS'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** CCM
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on      AND
115100030721     C                             PiVal<=9999999 AND
115200030721     C                             PiVal>*zeros
115300030721     C                   Z-ADD     PiVal         VABCCM
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      VABCCM = *zeros
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800040510     C                             + ' ' + 'VABCCM'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** LNP
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on    AND
116600030721     C                             PiVal<=999   AND
116700030721     C                             PiVal>*zeros
116800030721     C                   Z-ADD     PiVal         VABLNP
116900030721     C                   ELSE
117000040715     C                   SETON                                        32
117100030721     C                   EVAL      VABLNP = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300040510     C                             + ' ' + 'VABLNP'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** NRS
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on AND
118100030721     C                             PiVal<=99
118200030721     C                   Z-ADD     PiVal         VABNRS
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABNRS'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** LNA
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on    AND
119400030721     C                             PiVal<=999   AND
119500030721     C                             PiVal>*zeros
119600030721     C                   Z-ADD     PiVal         VABLNA
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABLNA = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABLNA'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** CTR
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on  AND
120900030721     C                             PiVal<=999
121000030721     C                   Z-ADD     PiVal         VABCTR
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABCTR'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** IAS
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiNum=*on
122200030721     C                   Z-ADD     PiVal         VABIAS
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABIAS'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** VLB
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiNum=*on
123400030721     C                   Z-ADD     PiVal         VABVLB
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABVLB'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** QFT
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiNum=*on
124600030721     C                   Z-ADD     PiVal         VABQFT
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABQFT'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** NCD
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on      AND
125800030721     C                             PiVal<=9999999
125900030721     C                   Z-ADD     PiVal         VABNCD
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABNCD'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** NCA
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on      AND
127100030721     C                             PiVal<=9999999
127200030721     C                   Z-ADD     PiVal         VABNCA
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABNCA'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** ZNC
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on AND
128400030721     C                             PiVal<=99
128500030721     C                   Z-ADD     PiVal         VABZNC
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABZNC'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** DCR
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on       AND
129700030721     C                             PiVal<=99999999
129800030721     C                   Z-ADD     PiVal         VABDCR
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABDCR'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** HCR
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on   AND
131000030721     C                             PiVal<=9999
131100030721     C                   Z-ADD     PiVal         VABHCR
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABHCR'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** VMD
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiNum=*on
132300030721     C                   Z-ADD     PiVal         VABVMD
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABVMD'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** ANT
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on        AND
133500030721     C                             PiVal<=999999999
133600030721     C                   Z-ADD     PiVal         VABANT
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABANT'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** CMO
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on
134800030721     C                   Z-ADD     PiVal         Num5_0
134900030721     C                   MOVEL(p)  Num5_0        VABCMO
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      VABCMO = *zeros
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABCMO'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700040510     C*
135800040510     C* Reperisco i campi del FIVAT
135900040510     C*** VATNOT
136000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
136100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
136200040510     C                   ENDIF
136300040510     C*** VATTRC
136400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
136500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
136600040510     C                   ENDIF
136700060331     C*** VATNOT_A
136800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
136900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137000060331     C                   ENDIF
137100060331     C*** VATNOT_B
137200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
137300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
137400060331     C                   ENDIF
137500060331     C*** VATNOT_E
137600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
137700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
137800060331     C                   ENDIF
137900070823     C*
138000071029     C***  ===>  Gestione campi "particolari" dentro ciclo
138100071204     C*
138200080411     C*** RMN
138300080411     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
138400080411     C                   EVAL      PiStr=VABRMA
138500080411     C                   EXSR      CHKNUM
138600080411     C                   IF        PiInt=*on
138700080411     C                   Z-ADD     PiVal         VABRMN
138800080411     C                   ELSE
138900080411     C                   SETON                                        32
139000080411     C                   EVAL      VABRMN = *zeros
139100080411     C                   EVAL      vinmsg = %trimr(vinmsg)
139200080411     C                             + ' ' + 'VABRMN'
139300080411     C                   ENDIF
139400080411     C                   ENDIF
139500071204     C*
139600070925     C*
139700071029     C***  <===  -----------------------------------------
139800040510     C*
139900030715     C                   ENDIF
140000030715     C                   EVAL      i = i + 1
140100030715     C                   ENDDO
140200070925     C*
140300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
140400080219     C*
140500080219     C* Considerazioni contrassegno
140600080219     C                   IF        VABCAS = *zeros
140700080219     C                   EVAL      VABTIC = *blanks
140800080219     C                   ENDIF
140900070925     C*
141000071029     C***  <===  -----------------------------------------
141100070925     C*
141200070925     C* Considerazioni finali su CBO/CAS
141300080219     C                   IF        VABCAS > *zeros
141400070925     C                   IF        VABCBO = '1'
141500070925     C                   EVAL      VABCBO = '4'
141600070925     C                   ENDIF
141700070925     C                   IF        VABCBO = '2'
141800070925     C                   EVAL      VABCBO = '6'
141900070925     C                   ENDIF
142000070925     C                   ENDIF
142100040510     C*
142200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
142300040510     C                   IF        VABNSP = *zeros
142400040510     C                   SETON                                        31
142500040510     C                   ENDIF
142600040715     C*
142700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
142800040715     C                   IF        VABLNP = *zeros
142900040715     C                   SETON                                        31
143000040715     C                   ENDIF
143100040510     C*
143200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
143300040510     C                   IF        VABRMN = *zeros
143400040510     C                   Z-ADD     VABNSP        VABRMN
143500040510     C                   ENDIF
143600080225     C*
143700080225     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
143800080225     C                   IF        VABRMA = *blanks
143900080225     C                   MOVE(P)   VABRMN        VABRMA
144000080225     C                   ENDIF
144100040128     C*
144200040510     C* Se provincia nn valorizzata la reperisco
144300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
144400040510     C                   IF        VABPRD = *blanks
144500040128     C                   CLEAR                   TISI95DS
144600040128     C                   EVAL      I95TCN = '3'
144700040128     C                   Z-ADD     datcor        I95DAT
144800040128     C                   EVAL      I95NAR = VABNZD
144900040128     C                   EVAL      I95CAP = VABCAD
145000040128     C                   EVAL      I95LOC = VABLOD
145100040128     C                   CALL      'TISI95R'
145200040128     C                   PARM                    TISI95DS
145300040128     C                   EVAL      VABPRD = O95PRV
145400040510     C                   ENDIF
145500040113     C*
145600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
145700040113     C                   IF        VABCAS > *zeros
145800040119     C                   EVAL      VABVCA = 'EUR'
145900040119     C                   IF        VABCBO = '1'
146000040113     C                   EVAL      VABCBO = '4'
146100040119     C                   ENDIF
146200040119     C                   IF        VABCBO = '2'
146300040119     C                   EVAL      VABCBO = '6'
146400040119     C                   ENDIF
146500040113     C                   ENDIF
146600020204     C*
146700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
146800020204     C                   EXSR      CHKIMPDIV
146900030715     C                   ENDIF
147000020204     C*
147100010607     C                   ENDSR
147200010607     C*----------------------------------------------------*
147300010601
147400020204
147500020204     C*----------------------------------------------------*
147600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
147700020204     C*----------------------------------------------------*
147800020204     C     CHKIMPDIV     BEGSR
147900020204     C*
148000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
148100020204     C                   Z-ADD     *zeros        wrkDec            9 9
148200020204     C*
148300020204     C* Come prima cosa effettuo considerazioni sulla divisa
148400020204     C                   IF        vabIAS > *zeros
148500020204     C                   IF        vabVAS <> 'EUR'
148600020204     C                   EVAL      vabVAS =  'ITL'
148700020204     C                   ENDIF
148800020204     C                   ENDIF
148900020204     C*
149000020204     C                   IF        vabCAS > *zeros
149100020204     C                   IF        vabVCA <> 'EUR'
149200020204     C                   EVAL      vabVCA =  'ITL'
149300020204     C                   ENDIF
149400020204     C                   ENDIF
149500020204     C*
149600020204     C                   IF        vabVMD > *zeros
149700020204     C                   IF        vabVAD <> 'EUR'
149800020204     C                   EVAL      vabVAD =  'ITL'
149900020204     C                   ENDIF
150000020204     C                   ENDIF
150100020204     C*
150200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
150300020204     C                   Z-ADD     vabIAS        wrkDec
150400020204     C                   IF        wrkDec > *zeros
150500020204     C                   IF        vabVAS = 'ITL'
150600020204     C                   EVAL      vabIAS = *zeros
150700020204     C                   ENDIF
150800020204     C                   ENDIF
150900020204     C*
151000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
151100020204     C                   Z-ADD     vabCAS        wrkDec
151200020204     C                   IF        wrkDec > *zeros
151300020204     C                   IF        vabVCA = 'ITL'
151400020204     C                   EVAL      vabCAS = *zeros
151500020204     C                   ENDIF
151600020204     C                   ENDIF
151700020204     C*
151800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
151900020204     C                   Z-ADD     vabVMD        wrkDec
152000020204     C                   IF        wrkDec > *zeros
152100020204     C                   IF        vabVAD = 'ITL'
152200020204     C                   EVAL      vabVMD = *zeros
152300020204     C                   ENDIF
152400020204     C                   ENDIF
152500020204     C*
152600020204     C                   ENDSR
152700020204     C***
152800020204
152900010330
153000010330
153100010330     C*----------------------------------------------------*
153200010330     C*  CONTROLLO NUMERICITA' CAMPI
153300010330     C*----------------------------------------------------*
153400010330     C     CHKNUM        BEGSR
153500010330     C*
153600010606     C                   IF        PiDecChr = *blanks
153700030715     C                   EVAL      PiDecChr = CharNUM
153800010606     C                   ENDIF
153900010606     C*
154000010606     C                   CALL(e)   'ISNUMERIC'
154100010330     C                   PARM                    PiStr            30
154200010606     C                   PARM                    PiDecChr          1
154300010330     C                   PARM      *ZEROS        PiVal            30 9
154400010330     C                   PARM      '0'           PiInt             1
154500010330     C                   PARM      '0'           PiNum             1
154600010330     C                   IF        %error
154700010606     C                   EVAL      PiNum=*off
154800010330     C                   ENDIF
154900010330     C*
155000010330     C                   ENDSR
155100010330     C***
155200010330
155300010601
155400010601
155500010601
155600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
155700010601     C     repfil        BEGSR
155800010601     C*
155900010601     C                   if        invfil = *zeros and
156000010601     C                             depfil > *zeros and
156100010629     C                             (vinflg = *blanks or
156200010629     C                              vinflg = *zeros)
156300010601     C
156400010601     C                   eval      invfil = depfil
156500010601     C                   endif
156600010601     C*
156700010601     C                   if        depfil <> invfil and
156800010601     C                             invfil > *zeros
156900010601     C                   eval      flgMulti = '1'
157000010601     C                   if        vinflg = *blanks
157100010601     C                   add       1             cntNonEl
157200010601     C                   endif
157300010601     C                   endif
157400010601     C*
157500010601     C                   if        vinflg = '2'
157600010601     C                   eval      flgStato = '2'
157700010601     C                   endif
157800010601     C*
157900010601     C                   ENDSR
158000010601     C***
158100010601
158200010601
158300010601
158400010330
158500040119      /TITLE Invio dei dati al punto operativo.
158600040119     C     invio         BEGSR
158700040119     C*
158800040119     C* 1° invio FIVAT
158900040119     C                   reset                   dscmz
159000040119     C                   move      vlrpoi        cmzdst
159100040119     C                   eval      cmzfld = 'FIVATWWR'
159200040119     C                   eval      cmzmbd = vlrhdl
159300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159400040119     C***                if        prmfir = *blanks
159500040119     C                   eval      cmzfla = 'FIVAT00F'
159600040119     C                   eval      cmzmba = 'FIVAT00F'
159700040119     C***                else
159800040119     C***                eval      cmzfla = prmfir
159900040119     C***                eval      cmzmba = prmfir
160000040119     C***                endif
160100040119     C                   eval      cmznrr = *zeros
160200040119     C                   move      §ctrokvt      cmznrr
160300040119     C                   eval      cmzlba = vlrfl1
160400040119     C                   call(e)   'TIS711C'
160500040119     C                   parm                    dscmz
160600040119     C                   parm      *blanks       esito
160700040119     C                   if        %error
160800040119     C                             or cmzerr = '1'
160900040119     C                             or esito  = '1'
161000040119     C                   eval      wrkesito = '3'
161100040119     C                   else
161200040119     C*
161300040119     C* 2° invio FIVAB
161400040119     C                   reset                   dscmz
161500040119     C                   move      vlrpoi        cmzdst
161600040119     C                   eval      cmzfld = vlrfou
161700040119     C                   eval      cmzmbd = vlrhdl
161800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161900040119     C***                if        prmfir = *blanks
162000040119     C                   eval      cmzfla = 'FIVAB00F'
162100040119     C                   eval      cmzmba = 'FIVAB00F'
162200040119     C***                else
162300040119     C***                eval      cmzfla = prmfir
162400040119     C***                eval      cmzmba = prmfir
162500040119     C***                endif
162600040119     C                   eval      cmznrr = *zeros
162700040119     C                   move      §ctrokvb      cmznrr
162800040119     C                   eval      cmzlba = vlrfl1
162900040119     C                   call(e)   'TIS711C'
163000040119     C                   parm                    dscmz
163100040119     C                   parm      *blanks       esito
163200040119     C                   if        %error
163300040119     C                             or cmzerr = '1'
163400040119     C                             or esito  = '1'
163500040119     C                   eval      wrkesito = '3'
163600040119     C                   endif
163700040119     C                   endif
163800040119     C*
163900040119     C                   ENDSR
164000040119     C***
164100010601
164200010601
164300010601
164400010601
164500010601      /TITLE Invio dei dati al punto operativo.
164600010601     C     opeini        BEGSR
164700010601     C*
164800010601     C* Inizializzo flag e contatori operativi
164900010601     C                   movel     '0'           flgGiro           1
165000010601     C                   movel     '0'           flgMulti          1
165100010601     C                   movel     '1'           flgStato          1
165200010615     C                   movel     '0'           flgOk             1
165300010601     C                   z-add     *zeros        cntNonEl         10 0
165400010601     C                   z-add     *zeros        depfil            3 0
165500010601     C                   z-add     *zeros        invfil            3 0
165600010601     C*
165700010601     C                   ENDSR
165800010601     C***
165900070326
166000070326
166100070326
166200070326
166300070326     C     *pssr         BEGSR
166400070329     C*
166500070329     C                   if        %open(tivin00r)
166600070329     C                   close     tivin00r
166700070329     C                   endif
166800070329     C                   if        %open(fivabwwr)
166900070329     C                   close     fivabwwr
167000070329     C                   endif
167100070329     C                   if        %open(fivatwwr)
167200070329     C                   close     fivatwwr
167300070329     C                   endif
167400070326     C*
167500070326     C* Effettuo la chiamata al CLLE preposto
167600070326     C                   call(e)   'TITVVTC'
167700070326     C                   parm                    parccm
167800070326     C                   parm                    parmbr
167900070326     C                   parm      '2'           paropz
168000070326     C*
168100070326     C                   eval      wrkesito = '2'
168200070404     C*
168300070404     C                   seton                                        LR
168400070326     C*
168500070326     C                   ENDSR     '*CANCL'
168600070326     C***
168700070326
168800070326
168900010330
169000010330
169100000613     C     *inzsr        BEGSR
169200990910     C*
169300990910     C     *entry        plist
169400990920     C                   parm                    tivlrds
169500990921     C                   parm      wrkesito      esito
169600000724     C                   parm                    prmlit
169700000710     C                   parm                    prmfir
169800010330     C*
169900010330     C* CALCOLA LA DATA CORRENTE
170000010330     C                   time                    wn14             14 0
170100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
170200010330     C                   z-add     wn8           g08dat
170300010330     C                   z-add     *zeros        g08inv
170400010330     C                   movel     '0'           g08err
170500010330     C                   call      'XSRDA8'
170600010330     C                   parm                    wlbda8
170700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
170800000613     C*
170900000613     C                   ENDSR
171000000613     C***
171100990908
171200080325     Otitv1e9p  E            riepilogo         2
171300990915     O                                              'Upload via Internet'
171400990915     O                                           +1 'Traduzione TIVIN00R -'
171500021025     O                                           +1 'FIVABWWR'
171600990915     O                                           +1 'Testate bolle'
171700990915     O          E            riepilogo   2
171800990915     O                       wrkdata
171900990915     O                       wrkora              +1
172000990915     O                       procname            +1
172100990915     O          E            riepilogo   2
172200990915     O                                              'Cliente..................:'
172300010605     O                       VABCCM        z     +1
172400990915     O          E            riepilogo   2
172500990920     O                                              'Riferimento Strategi.....:'
172600990920     O                       vlrhdl              +1
172700990915     O          E            riepilogo   2
172800990915     O                                              'Giusti...................:'
172900040119     O                       §CTROKVB      2   +  1
173000990915     O          E            riepilogo   2
173100990915     O                                              'Sbagliati e corretti.....:'
173200971022     O                       §CTRMO        2   +  1
173300990915     O          E            riepilogo   2
173400990915     O                                              'Sbagliati e scartati.....:'
173500971022     O                       §CTRNO        2   +  1
173600000613
173700080325     Otitv1e9ps E            testdett          2
173800000613     O                                              'Upload via Internet'
173900000613     O                                           +1 'Traduzione TIVIN00R -'
174000021025     O                                           +1 'FIVABWWR'
174100000613     O                                           +1 'Testate bolle'
174200000616     O          E            testdett    3
174300000613     O                                           +2 'N° rec'
174400000613     O                                           +3 'Anteprima contenuto'
174500000616     O          E            rigadett    2
174600000613     O                       rrnum               +2
174700000621     O                       recko               +3
174800000616     O          E            findett     2
174900000613     O                       wrkdata
175000000613     O                       wrkora              +1
175100000613     O                       procname            +1
175200000616     O          E            findett     2
175300000613     O                                              'Cliente..................:'
175400010605     O                       VABCCM        z     +1
175500000616     O          E            findett     2
175600000613     O                                              'Riferimento Strategi.....:'
175700000613     O                       vlrhdl              +1
175800000616     O          E            findett     2
175900000613     O                                              'Giusti...................:'
176000040119     O                       §CTROKVB      2   +  1
176100000616     O          E            findett     2
176200000613     O                                              'Sbagliati e corretti.....:'
176300000613     O                       §CTRMO        2   +  1
176400000616     O          E            findett     2
176500000613     O                                              'Sbagliati e scartati.....:'
176600000613     O                       §CTRNO        2   +  1
176700000613** CMD - COMANDI CL
176800080325OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1E9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
176900080325OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1E9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
177000080325DLTOVR FILE(TITV1E9P TITV1E9PS) LVL(*)
177100010606
177200000613
177300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
177400080325VABRSD;FILL01;FILL02;FILL03;VABRD2;VABIND;VABCAD;VABLOD;
177500080325VABNOT;VABNSP;FILL05;FILL06;VABNCL;VABRMA;FILL08;
177600080325VABCAS;FILL09;VATNOT_B;VATNOT_A;FILL10;FILL11;VABPRD;VABPKB
