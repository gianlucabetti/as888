000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800040122     Ftitv92p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100040122     Ftitv92ps  O    f  198        PRINTER usropn
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
005300090324     D CharSOS         S              1
005400030715     D posDa           S              3  0 INZ(*zeros)
005500030715     D posA            S              3  0 INZ(*zeros)
005600030715     D i               s              3  0 INZ(1)
005700030715     D wGiro           s              1  0 INZ(*zeros)
005800060331     D wVATNOT_A       s                   LIKE(VATNOT)
005900060331     D wVATNOT_B       s                   LIKE(VATNOT)
006000060331     D wVATNOT_E       s                   LIKE(VATNOT)
006100060331     D wVABNSP         s                   LIKE(VABNSP) INZ
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
008200010330
008300010330
008400010330
008500990915     C                   time                    wrkdata
008600990915     C                   time                    wrkora
008700000913     C                   reset                   rrnum
008800990921     C                   reset                   esito
008900990921     C                   reset                   wrkesito
009000010601     C*
009100010601     C                   exsr      opeini
009200010605     C                   exsr      rwvab
009300010601     C*
009400010601     C                   seton                                        lr
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010000010601     C*--------------------------------------------------------
010100010601     C     PREELA        BEGSR
010200010601     C*
010300010601     C* SE OCCORRE SPEDIRE IN FILIALE
010400010601     C                   if        invfil <> *zeros and
010500010601     C                             flgGiro = '0'
010600010601     C*
010700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010800010601     C                   eval      flgGiro = '1'
010900000724     C*
011000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011200010601     C     invfil        chain     azorg01l
011300000724     C                   if        %found
011400000724     C                   movel(p)  CMD(1)        depcmd
011500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011600000724     C*
011700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012000000724     C                   PARM                    depcmd
012100000724     C                   PARM                    LENGH
012200000724     C*
012300000724     C                   endif
012400000616     C*
012500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012600000616     C                   movel(p)  CMD(2)        depcmd
012700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012800000616     C*
012900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013000011113     C                   Z-ADD     150           LENGH            15 5
013100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200000616     C                   PARM                    depcmd
013300000616     C                   PARM                    LENGH
013400000613     C*
013500040122     C                   if        not %open(titv92ps)
013600040122     C                   open      titv92ps
013700070329     C                   except    testdett
013800000613     C                   endif
013900010601     C*
014000010601     C                   endif
014100010601     C*
014200010601     C                   ENDSR
014300010601     C***
014400010601
014500010601
014600010601
014700010601
014800010601     C*--------------------------------------------------------
014900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015000010601     C*--------------------------------------------------------
015100010601     C     ENDELA        BEGSR
015200990915     C*
015300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015400040119     C*
015500040119     C* Effettuo la chiamata al CLLE preposto
015600040119     C                   call(e)   'TITVVTC'
015700040510     C                   parm                    parccm
015800040119     C                   parm                    parmbr
015900040119     C                   parm      '2'           paropz
016000050201     C*
016100050201     C* Effettuo lancio TISI95 solo x chiusura
016200050201     C                   CLEAR                   TISI95DS
016300050201     C                   EVAL      I95TLA = 'C'
016400050201     C                   CALL      'TISI95R'
016500050201     C                   PARM                    TISI95DS
016600000613     C*
016700040122     C                   if        %open(titv92ps)
016800000616     C                   except    findett
016900040122     C                   close     titv92ps
017000000613     C                   endif
017100000616     C*
017200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017300011113     C                   Z-ADD     150           LENGH            15 5
017400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017500000616     C                   PARM                    CMD(3)
017600000616     C                   PARM                    LENGH
017700000616     C*
017800010601     C                   ENDSR
017900010601     C***
018000010601
018100010601
018200010601
018300000613
018400000613     C*--------------------------------------------------------
018500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018600000613     C*--------------------------------------------------------
018700000621     C     STPR          BEGSR
018800000613     C*
018900040122     C                   if        not %open(titv92p)
019000040122     C   50              open      titv92p
019100990915     C                   endif
019200990915     C*
019300030715     C   50              except    riepilogo
019400990915     C*
019500040122     C                   if        %open(titv92p)
019600040122     C   50              close     titv92p
019700990914     C                   endif
019800000613     C*
019900000613     C                   ENDSR
020000000613     C***
020100000613
020200010601
020300010601
020400010330     C*--------------------------------------------------------
020500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020600010330     C*--------------------------------------------------------
020700010605     C     RWVAB         BEGSR
020800010330     C*
020900010330     C                   if        not %open(tivin00r)
021000010330     C                   open      tivin00r
021100010330     C                   endif
021200021025     C                   if        not %open(fivabwwr)
021300021025     C                   open      fivabwwr
021400010330     C                   endif
021500040119     C*
021600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021700040119     C                   exsr      prevat
021800040119     C*
021900040119     C                   if        chkcall = '0'
022000040119     C*
022100040119     C                   if        not %open(fivatwwr)
022200040119     C                   open      fivatwwr
022300040119     C                   endif
022400010330     C*
022500040119     C                   clear                   §CTROKVB          7 0
022600040119     C                   clear                   §CTROKVT          7 0
022700040119     C                   clear                   §CTRMO            7 0
022800040119     C                   clear                   §CTRNO            7 0
022900010330     C*
023000030822     C                   do        *HIVAL
023100010330     C*
023200030822     C                   read      tivin00r                               70
023300010618     C*
023400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023500010618     C                   if        vinflg = '1'
023600010618     C                   eval      flgOk = '1'
023700010618     C                   endif
023800040510     C*
023900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024000040510     C                   z-add     *zeros        wLenVINDTA        4 0
024100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024200090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
024300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024400090403     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024500040510     C                   z-add     1             wFlgVINDTA
024600040510     C                   leave
024700040510     C                   endif
024800040510     C                   enddo
024900010618     C*
025000040510     C                   if        vindta > *blanks AND
025100040510     C                             wFlgVINDTA = 1
025200010330     C                   add       1             rrnum
025300010330     C*
025400010601     C                   if        *in70 = *off and
025500010330     C                             (vinflg = *blanks
025600010330     C                              or vinflg = '0'
025700010330     C                              or vinflg = '2')
025800010330     C*
025900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026000010711     C                   if        vinflg = *blanks or vinflg = '0'
026100010711     C                   clear                   vinmsg
026200010711     C                   endif
026300010601     C*
026400081014     C                   exsr      inzvar
026500090324     C*
026600090324     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
026700090324     C* separatori campi
026800090324     C                   if        CharTXT <> *blanks
026900090324     C                   z-add     *zeros        wLenVINDTA        4 0
027000090324     C                   z-add     *zeros        wFlgVINDTA        1 0
027100090324     C                   setoff                                       80
027200090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
027300090324     C                   eval      wLenVINDTA = wLenVINDTA + 1
027400090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
027500090324     C                   if        not *in80
027600090324     C                   seton                                        80
027700090324     C                   else
027800090324     C                   setoff                                       80
027900090324     C                   endif
028000090324     C                   endif
028100090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
028200090324     C                             and *in80 = *on
028300090324     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
028400090324     C                   endif
028500090324     C                   enddo
028600090324     C                   endif
028700090324     C*
028800010605     C                   exsr      impvab
028900010601     C*
029000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029100010605     C                   eval      depfil = VABLNP
029200010601     C                   exsr      repfil
029300010601     C                   if        depfil = invfil
029400021025     C                   if        vlrpoi = 999
029500030822     C                   move(P)   invfil        VABFGS
029600070704     C                   move(P)   invfil        winvcmz           3
029700021025     C                   else
029800030822     C                   move(P)   vlrpoi        VABFGS
029900070704     C                   move(P)   vlrpoi        winvcmz
030000021025     C                   endif
030100010601     C*
030200010601     C                   exsr      PREELA
030300010601     C*
030400010604     C* Ebbene...
030500010604     C*
030600040119     C  N31              add       1             §CTROKVB
030700040119     C   32              add       1             §CTRMO
030800040119     C   31              add       1             §CTRNO
030900030822     C                   if        wGiro = 2
031000040510     C* Scrivo sempre il VAT
031100040510     C  N31              exsr      wrivat
031200040510     C* Scrivo il VAB solo a rottura d spedizione
031300040119     C                   if        depspe =  *zeros OR
031400040119     C                             depspe <> vabNSP
031500040119     C  N31              exsr      wrivab
031600040119     C  N31              eval      depspe = vabNSP
031700040119     C                   endif
031800040119     C                   endif
031900010604     C*
032000010604     C                   if        *in31 = *off and
032100010604     C                             *in32 = *off
032200010604     C                   eval      vinflg = '1'
032300010604     C                   else
032400010604     C                   eval      recko = vindta
032500010604     C                   except    rigadett
032600010604     C                   eval      vinflg = '2'
032700010604     C                   endif
032800010604     C                   endif
032900010601     C*
033000010604     C                   endif
033100010604     C*
033200010330     C                   else
033300010330     C                   eval      vinflg = '1'
033400010330     C                   endif
033500010601     C*
033600010601     C  N70              update    tivin000
033700010330     C*
033800030822     C  N70              enddo
033900040119     C*
034000040119     C                   endif                                                  (endif - chkcall)
034100010601     C*
034200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034300010601     C                   if        cntNonEl = *zeros or
034400010601     C                             flgMulti = '0'
034500010330     C* Se non ci sono record con errori ...
034600010601     C                   if        §ctrno = 0 and
034700010604     C                             §ctrmo = 0 and
034800010601     C                             flgStato <> '2'
034900010330     C* ... restituisco esito OK.
035000010330     C                   eval      wrkesito = '0'
035100010330     C                   else
035200040119     C                   if        §ctrokvb > 0
035300010330     C                   eval      wrkesito = '1'
035400010330     C                   else
035500010615     C                   if        flgOk = '0'
035600010615     C                   eval      wrkesito = '2'
035700010615     C                   else
035800010615     C                   eval      wrkesito = '6'
035900010615     C                   endif
036000010330     C                   endif
036100010330     C                   endif
036200010601     C                   else
036300010601     C                   eval      wrkesito = '9'
036400010601     C                   endif
036500010330     C*
036600010330     C                   if        %open(tivin00r)
036700010330     C                   close     tivin00r
036800010330     C                   endif
036900021025     C                   if        %open(fivabwwr)
037000021025     C                   close     fivabwwr
037100010330     C                   endif
037200040512     C                   if        %open(fivatwwr)
037300040512     C                   close     fivatwwr
037400040512     C                   endif
037500010601     C*
037600010601     C                   if        vlrpoi <> 999
037700010601     C                   eval      invfil = vlrpoi
037800010601     C                   endif
037900010330     C*
038000040119     C                   if        §ctrokvb > 0
038100010601     C                             and invfil > *zeros
038200010330     C                   exsr      invio
038300010330     C                   endif
038400010601     C*
038500010618     C                   if        flgGiro = '1'
038600010601     C                   exsr      endela
038700010618     C                   endif
038800010330     C*
038900010330     C                   ENDSR
039000010330     C***
039100040119
039200040119
039300040119
039400040119     C*----------------------------------------------------*
039500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039600040119     C*----------------------------------------------------*
039700040510     C     WRIVAB        BEGSR
039800040119     C*
039900060331     C* Gestisco l'eventuale rottura x numero spedizione
040000060331     C                   if        VABNSP <> wVABNSP
040100070301     C                   if        wVATNOT_E <> *blanks
040200110419     C                   if        VABCTM = *blanks
040300070301     C                   eval      VABCTM = '7Q'
040400110419     C                   endif
040500070301     C                   endif
040600040119     C                   write     fivab000                                     => scarico il VAB
040700060331     C                   eval      wVABNSP = VABNSP
040800060331     C                   endif
040900040119     C*
041000040119     C                   ENDSR
041100040119
041200040119
041300040119
041400040119     C*----------------------------------------------------*
041500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041600040119     C*----------------------------------------------------*
041700040510     C     WRIVAT        BEGSR
041800060512     C*
041900060512     C* Inizializzo i campi chiave
042000060512     C                   eval      VATFGS = VABFGS
042100060512     C                   eval      VATAAS = VABAAS
042200060512     C                   eval      VATCCM = VABCCM
042300060512     C                   eval      VATNSP = VABNSP
042400060512     C                   eval      VATLNP = VABLNP
042500040714     C*
042600040714     C* Solo se VATNOT valorizzato scarico il buffer
042700040714     C                   if        VATNOT <> *blanks
042800040119     C*
042900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
043000040510     C                   if        VATTRC = *blanks
043100040510     C                   eval      VATTRC = 'E'
043200040510     C                   endif
043300040510     C                   write     FIVAT000
043400040119     C                   add       1             §CTROKVT
043500040714     C*
043600040714     C                   endif
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
045300060331     C                   if        wVATNOT_E <> *blanks
045400060331     C                   eval      VATNOT = wVATNOT_E
045500060331     C                   eval      VATTRC = 'E'
045600060331     C                   write     FIVAT000
045700060331     C                   add       1             §CTROKVT
045800060331     C                   endif
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
049700090403     C                   z-add     *zeros        invfil
049800060331     C                   MOVEL     *blanks       wVATNOT_A
049900060331     C                   MOVEL     *blanks       wVATNOT_B
050000060331     C                   MOVEL     *blanks       wVATNOT_E
050100081014     C*
050200081014     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050300081014     C                   CLEAR                   FIVAB000
050400081014     C                   CLEAR                   FIVAT000
050500030822     C*
050600081014     C* Inizializzo la schiera d wrk x i dati
050700030822     C                   CLEAR                   SkSplitCSV
050800030822     C                   EVAL      i = 1
050900030822     C                   EVAL      posDa = *zeros
051000030822     C                   EVAL      posA  = *zeros
051100030822     C*
051200030822     C* Reimposto i valori di default
051300030822     C                   EXSR      DEFCAM
051400030822     C*
051500010330     C                   ENDSR
051600010330     C*----------------------------------------------------*
051700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051800010330     C*----------------------------------------------------*
051900010330     C     DEFCAM        BEGSR
052000030822     C*
052100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052200030715     C* e delimitatore testo.
052300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052600090324     C*
052700090324     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
052800090324     C                   EVAL      CharSOS = CharNUM
052900030822     C*
053000081007     C* Reperisco il parametro che mi indica se tuare o meno la stampa in filiale
053100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
053200081007     C                   SETON                                        40
053300030715     C                   ELSE
053400081007     C                   SETOFF                                       40
053500030715     C                   ENDIF
053600081007     C*
053700081007     C* Imposto sempre e cmq che la stampa NN vada in filiale
053800081007     C                   SETOFF                                       50
053900030822     C*
054000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054100030822     C                   EVAL      posDaDft = 1
054200030822     C                   EVAL      posADft  = 0
054300030822     C                   EVAL      wGiroDft = 0
054400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054500030822     C                             posDaDft > 0
054600030822     C*
054700030822     C* Gestisco il 1° giro
054800030822     C                   IF        wGiroDft = 0
054900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055100030822     C* Incremento il contatore dei "giri"
055200030822     C                   EVAL      wGiroDft = 1
055300030822     C                   ELSE
055400030822     C                   EVAL      posDaDft = posADft
055500030822     C                   ENDIF
055600030822     C* Eseguo lo scan x trovare la fine del campo corrente
055700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055800030822     C*
055900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056100030822     C                   IF        posDaDft > 0 AND
056200030822     C                             posADft  > 0
056300030822     C* NCL
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'NCL'
056800030822     C                   EVAL      PiStr=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   EXSR      CHKNUM
057200030822     C                   IF        PiInt=*on
057300030822     C                   Z-ADD     PiVal         VABNCL
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* CCM
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'CCM'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABCCM
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* LNP
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'LNP'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABLNP
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* NRS
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'NRS'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABNRS
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* CTR
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'CTR'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABCTR
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* PKB
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'PKB'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiNum=*on
063800030822     C                   Z-ADD     PiVal         VABPKB
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* VLB
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'VLB'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiNum=*on
065100030822     C                   Z-ADD     PiVal         VABVLB
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* QFT
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'QFT'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiNum=*on
066400030822     C                   Z-ADD     PiVal         VABQFT
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* CBO
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'CBO'
067200030822     C                   EVAL      VABCBO=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* TSP
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'TSP'
068100030822     C                   EVAL      VABTSP=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* VAS
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'VAS'
069000030822     C                   EVAL      VABVAS=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* VCA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'VCA'
069900030822     C                   EVAL      VABVCA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
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
078400080516     C* TIC
078500080516     C                   IF        %subst(
078600080516     C                             %subst(vlrppt:posDaDft+1:
078700080516     C                             posADft-posDaDft-1):1:3)
078800080516     C                             = 'TIC'
078900080516     C                   EVAL      VABTIC=%trim(%subst(
079000080516     C                             %subst(vlrppt:posDaDft+1:
079100080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200080516     C                   ENDIF
079300040714     C* VATTRC
079400040714     C                   IF        %subst(
079500040714     C                             %subst(vlrppt:posDaDft+1:
079600040714     C                             posADft-posDaDft-1):1:3)
079700040714     C                             = 'TRC'
079800040714     C                   EVAL      VATTRC=%trim(%subst(
079900040714     C                             %subst(vlrppt:posDaDft+1:
080000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100040714     C                   ENDIF
080200030822     C* ...
080300030822     C                   ENDIF
080400030822     C                   ENDDO
080500020204     C*
080600010330     C                   ENDSR
080700010607     C*----------------------------------------------------*
080800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080900010607     C*----------------------------------------------------*
081000010607     C     IMPVAB        BEGSR
081100010607     C*
081200010607     C                   SETOFF                                       3132
081300030515     C*
081400010607     C                   MOVEL     datcor        VABAAS
081500010607     C                   MOVE      datcor        VABMGS
081600010607     C*
081700030715     C**********
081800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081900030715     C**********
082000030715     C                   IF        wGiro = *zeros
082100040113     C*
082200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082400040510     C                             %trim(intcol(3))<>*blanks
082500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082600040113     C                                      %trim(intcol(3))
082700040510     C                   ENDIF
082800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082900031201     C     minu:maiu     XLATE     vindta        vindta
083000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083100030715     C                   DOW       posDa <= %len(%trim(vindta))
083200030715     C*
083300030715     C* Gestisco il 1° campo
083400030715     C                   IF        i = 1
083500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083600030715     C                   EVAL      posDa = 1
083700030715     C* Eseguo lo scan x trovare la fine del primo campo
083800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C* X i campi successivi al 1°
084300030715     C                   ELSE
084400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084500030715     C                   EVAL      posDa = posA + 1
084600030715     C* Eseguo lo scan x trovare la fine del campo corrente
084700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084800030715     C* Gestisco l'ultimo campo
084900030715     C                   IF        posA = *zeros
085000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085100030715     C* A questo punto "estraggo" il campo corrente
085200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085300030715     C                                                    (posA-posDa))
085400030715     C                   LEAVE
085500030715     C                   ELSE
085600030715     C* A questo punto "estraggo" il campo corrente
085700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085800030715     C                                                    (posA-posDa))
085900030715     C                   ENDIF
086000030715     C                   ENDIF
086100030715     C* Incremento il contatore d campo
086200030715     C                   EVAL      i = i +1
086300030715     C                   ENDDO
086400030715     C*
086500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086600030715     C* delimitazione testo
086700030715     C                   EVAL      i = 1
086800030715     C                   DOW       i < %elem(SkSplitFLD)
086900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087100030715     C                   EVAL      i = i + 1
087200030715     C                   ENDDO
087300030715     C                   EVAL      wGiro = 1
087400030715     C                   ELSE
087500030715     C                   EVAL      wGiro = 2
087600031204     C**********
087700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087800031204     C* d separatore campo
087900031204     C**********
088000031204     C                   EVAL      vindta = %trim(vindta)
088100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088200031204     C                   EVAL      vindta = %subst(vindta:2)
088300031204     C                   ENDDO
088400031204     C*
088500031204     C                   Z-ADD     *zeros        lunghInput        4 0
088600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089000031204     C                   ENDDO
089100030715     C**********
089200030715     C* Effettuo lo split del campo dati d input
089300030715     C**********
089400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089500030715     C                   DOW       posDa <= %len(%trim(vindta))
089600030715     C*
089700030715     C* Gestisco il 1° campo
089800030715     C                   IF        i = 1
089900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090000030715     C                   EVAL      posDa = 1
090100030715     C* Eseguo lo scan x trovare la fine del primo campo
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* A questo punto "estraggo" il campo corrente
090400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090500030715     C                                                    (posA-posDa))
090600030715     C* X i campi successivi al 1°
090700030715     C                   ELSE
090800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090900030715     C                   EVAL      posDa = posA + 1
091000031204     C* Verifico che nn vi sia il campo nn valorizzato
091100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091200031204     C* Se campo nn valorizzato skippo al prossimo
091300031204     C                   EVAL      SkSplitCSV(i) = *blanks
091400031204     C                   EVAL      posA  = posA + 1
091500031204     C                   ELSE
091600030715     C* Eseguo lo scan x trovare la fine del campo corrente
091700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091800030715     C* Gestisco l'ultimo campo
091900030715     C                   IF        posA = *zeros
092000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092100030715     C* A questo punto "estraggo" il campo corrente
092200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092300030715     C                                                    (posA-posDa))
092400030715     C                   LEAVE
092500030715     C                   ELSE
092600030715     C* A questo punto "estraggo" il campo corrente
092700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092800030715     C                                                    (posA-posDa))
092900030715     C                   ENDIF
093000031204     C                   ENDIF
093100030715     C                   ENDIF
093200030715     C* Incremento il contatore d campo
093300030715     C                   EVAL      i = i +1
093400030715     C                   ENDDO
093500030715     C*
093600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093700030715     C* delimitazione testo
093800030715     C                   EVAL      i = 1
093900030715     C                   DOW       i < %elem(SkSplitCSV)
094000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094200030715     C                   EVAL      i = i + 1
094300030715     C                   ENDDO
094400030715     C*
094500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094600030715     C                   EVAL      i = 1
094700030715     C                   DOW       i < %elem(SkSplitFLD)
094800030715     C*
094900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095100030715     C                             SkSplitCSV(i) <> *blanks
095200030721     C*** ATB
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095400030721     C                   EVAL      VABATB = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030715     C*** RSD
095700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096100030715     C* ==
096200030715     C                   ENDIF
096300030721     C*** RD2
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030715     C*** IND
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096900030715     C                   EVAL      VABIND = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** LOD
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** PRD
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030721     C*** NZD
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030715     C*** RMA
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** NOT
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** NT2
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** VCA
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** VAS
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030715     C*** FFD
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030721     C*** GC1
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101000100304     C                   IF        %subst(VABGC1:1) = 'P' or
101100100304     C                             %subst(VABGC1:1) = 'M'
101200100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
101300100304     C                   ENDIF
101400030721     C                   ENDIF
101500030721     C*** GC2
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101800100304     C                   IF        %subst(VABGC2:1) = 'P' or
101900100304     C                             %subst(VABGC2:1) = 'M'
102000100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
102100100304     C                   ENDIF
102200030721     C                   ENDIF
102300030721     C*** TSP
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102600030721     C                   ENDIF
102700040510     C*** CBO
102800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103000040510     C                   ENDIF
103100030721     C*** NAS
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** TIC
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** GCA
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** XCO
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** CTM
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** TCR
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** CTS
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** FTM
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** VAD
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** GMA
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GGA
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** GVA
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** TC1
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** TC2
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** SCL
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** RMO
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** NMO
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*
110000030721     C* Reperisco quindi i campi numerici...
110100040510     C*** NSP
110200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110300040510     C                   EVAL      PiStr=SkSplitCSV(i)
110400040510     C                   EXSR      CHKNUM
110500040510     C                   IF        PiInt=*on
110600040510     C                   Z-ADD     PiVal         VABNSP
110700040510     C                   ELSE
110800040510     C                   SETON                                        31
110900040510     C                   EVAL      VABNSP = *zeros
111000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111100040510     C                             + ' ' + 'VABNSP'
111200040510     C                   ENDIF
111300040510     C                   ENDIF
111400070703     C*** RMN
111500070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111600070703     C                   EVAL      PiStr=SkSplitCSV(i)
111700070703     C                   EXSR      CHKNUM
111800070703     C                   IF        PiInt=*on
111900070703     C                   Z-ADD     PiVal         VABRMN
112000070703     C                   ELSE
112100070703     C                   SETON                                        32
112200070703     C                   EVAL      vinmsg = %trimr(vinmsg)
112300070703     C                             + ' ' + 'VABRMN'
112400070703     C                   ENDIF
112500070703     C                   ENDIF
112600070705     C*** CAD
112700070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112800070705     C                   EVAL      VABCAD = SkSplitCSV(i)
112900070705     C                   ENDIF
113000030721     C*** NCL
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiInt=*on
113500081007     C  N40              Z-ADD     PiVal         VABNCL
113600081007     C   40              ADD       PiVal         VABNCL
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      VABNCL = *zeros
114000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114100040122     C                             + ' ' + 'VABNCL'
114200030721     C                   ENDIF
114300030721     C                   ENDIF
114400030721     C*** PKB
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiNum=*on
114900081007     C  N40              Z-ADD     PiVal         VABPKB
115000081007     C   40              ADD       PiVal         VABPKB
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABPKB = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABPKB'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** CAS
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiNum=*on
116300110518     C                   EVAL      PiVal = PiVal / 100
116400110518     C                   Z-ADD(H)  PiVal         VABCAS
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABCAS = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900030721     C                             + ' ' + 'VABCAS'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** CCM
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiInt=*on      AND
117700030721     C                             PiVal<=9999999 AND
117800030721     C                             PiVal>*zeros
117900030721     C                   Z-ADD     PiVal         VABCCM
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      VABCCM = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400040510     C                             + ' ' + 'VABCCM'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** LNP
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on    AND
119200030721     C                             PiVal<=999   AND
119300030721     C                             PiVal>*zeros
119400030721     C                   Z-ADD     PiVal         VABLNP
119500030721     C                   ELSE
119600040715     C                   SETON                                        32
119700030721     C                   EVAL      VABLNP = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040510     C                             + ' ' + 'VABLNP'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** NRS
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on AND
120700030721     C                             PiVal<=99
120800030721     C                   Z-ADD     PiVal         VABNRS
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABNRS'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** LNA
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on    AND
122000030721     C                             PiVal<=999   AND
122100030721     C                             PiVal>*zeros
122200030721     C                   Z-ADD     PiVal         VABLNA
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      VABLNA = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABLNA'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** CTR
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on  AND
123500030721     C                             PiVal<=999
123600030721     C                   Z-ADD     PiVal         VABCTR
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABCTR'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** IAS
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiNum=*on
124800030721     C                   Z-ADD     PiVal         VABIAS
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABIAS'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** VLB
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiNum=*on
126000081007     C  N40              Z-ADD     PiVal         VABVLB
126100081007     C   40              ADD       PiVal         VABVLB
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABVLB'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** QFT
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiNum=*on
127300081007     C  N40              Z-ADD     PiVal         VABQFT
127400081007     C   40              ADD       PiVal         VABQFT
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABQFT'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** NCD
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on      AND
128600030721     C                             PiVal<=9999999
128700030721     C                   Z-ADD     PiVal         VABNCD
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABNCD'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** NCA
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on      AND
129900030721     C                             PiVal<=9999999
130000030721     C                   Z-ADD     PiVal         VABNCA
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABNCA'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** ZNC
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on AND
131200030721     C                             PiVal<=99
131300030721     C                   Z-ADD     PiVal         VABZNC
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABZNC'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** DCR
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on       AND
132500030721     C                             PiVal<=99999999
132600030721     C                   Z-ADD     PiVal         VABDCR
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABDCR'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** HCR
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on   AND
133800030721     C                             PiVal<=9999
133900030721     C                   Z-ADD     PiVal         VABHCR
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABHCR'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** VMD
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiNum=*on
135100030721     C                   Z-ADD     PiVal         VABVMD
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABVMD'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** ANT
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on        AND
136300030721     C                             PiVal<=999999999
136400030721     C                   Z-ADD     PiVal         VABANT
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABANT'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** CMO
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiInt=*on
137600030721     C                   Z-ADD     PiVal         Num5_0
137700030721     C                   MOVEL(p)  Num5_0        VABCMO
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      VABCMO = *zeros
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200030721     C                             + ' ' + 'VABCMO'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500040510     C*
138600040510     C* Reperisco i campi del FIVAT
138700040510     C*** VATNOT
138800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139000040510     C                   ENDIF
139100040510     C*** VATTRC
139200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139400040510     C                   ENDIF
139500060331     C*** VATNOT_A
139600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139800060331     C                   ENDIF
139900060331     C*** VATNOT_B
140000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140200060331     C                   ENDIF
140300060331     C*** VATNOT_E
140400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140600060331     C                   ENDIF
140700040510     C*
140800030715     C                   ENDIF
140900030715     C                   EVAL      i = i + 1
141000030715     C                   ENDDO
141100040510     C*
141200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
141300040510     C                   IF        VABNSP = *zeros
141400040510     C                   SETON                                        31
141500040510     C                   ENDIF
141600040715     C*
141700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
141800040715     C                   IF        VABLNP = *zeros
141900040715     C                   SETON                                        31
142000040715     C                   ENDIF
142100040510     C*
142200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
142300040510     C                   IF        VABRMN = *zeros
142400040510     C                   Z-ADD     VABNSP        VABRMN
142500040510     C                   ENDIF
142600040510     C*
142700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
142800040510     C                   IF        VABRMA = *blanks
142900040714     C                   MOVEL     VABRMN        VABRMA
143000040510     C                   ENDIF
143100070705     C*
143200070705     C* Considerazioni particolari
143300070705     C*** NZD
143400070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
143500070705     C                   EVAL      VABNZD = *blanks
143600070705     C                   ENDIF
143700070705     C*** CAD
143800070705     C                   IF        VABNZD = *blanks
143900070705     C                   EVAL      PiStr=VABCAD
144000070705     C                   EXSR      CHKNUM
144100070705     C                   IF        PiInt=*on
144200070705     C                   Z-ADD     PiVal         Num5_0
144300070705     C                   MOVEL(p)  Num5_0        VABCAD
144400070705     C                   ELSE
144500070705     C                   SETON                                        32
144600070705     C                   EVAL      VABCAD = *zeros
144700070705     C                   EVAL      vinmsg = %trimr(vinmsg)
144800070705     C                             + ' ' + 'VABCAD'
144900070705     C                   ENDIF
145000070705     C                   ENDIF
145100040128     C*
145200040510     C* Se provincia nn valorizzata la reperisco
145300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
145400040510     C                   IF        VABPRD = *blanks
145500040128     C                   CLEAR                   TISI95DS
145600040128     C                   EVAL      I95TCN = '3'
145700040128     C                   Z-ADD     datcor        I95DAT
145800040128     C                   EVAL      I95NAR = VABNZD
145900040128     C                   EVAL      I95CAP = VABCAD
146000040128     C                   EVAL      I95LOC = VABLOD
146100040128     C                   CALL      'TISI95R'
146200040128     C                   PARM                    TISI95DS
146300040128     C                   EVAL      VABPRD = O95PRV
146400040510     C                   ENDIF
146500040113     C*
146600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
146700040113     C                   IF        VABCAS > *zeros
146800040119     C                   EVAL      VABVCA = 'EUR'
146900040119     C                   IF        VABCBO = '1'
147000040113     C                   EVAL      VABCBO = '4'
147100040119     C                   ENDIF
147200040119     C                   IF        VABCBO = '2'
147300040119     C                   EVAL      VABCBO = '6'
147400040119     C                   ENDIF
147500040113     C                   ENDIF
147600020204     C*
147700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
147800020204     C                   EXSR      CHKIMPDIV
147900030715     C                   ENDIF
148000020204     C*
148100010607     C                   ENDSR
148200010607     C*----------------------------------------------------*
148300010601
148400020204
148500020204     C*----------------------------------------------------*
148600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
148700020204     C*----------------------------------------------------*
148800020204     C     CHKIMPDIV     BEGSR
148900020204     C*
149000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149100020204     C                   Z-ADD     *zeros        wrkDec            9 9
149200020204     C*
149300020204     C* Come prima cosa effettuo considerazioni sulla divisa
149400020204     C                   IF        vabIAS > *zeros
149500020204     C                   IF        vabVAS <> 'EUR'
149600020204     C                   EVAL      vabVAS =  'ITL'
149700020204     C                   ENDIF
149800020204     C                   ENDIF
149900020204     C*
150000020204     C                   IF        vabCAS > *zeros
150100020204     C                   IF        vabVCA <> 'EUR'
150200020204     C                   EVAL      vabVCA =  'ITL'
150300020204     C                   ENDIF
150400020204     C                   ENDIF
150500020204     C*
150600020204     C                   IF        vabVMD > *zeros
150700020204     C                   IF        vabVAD <> 'EUR'
150800020204     C                   EVAL      vabVAD =  'ITL'
150900020204     C                   ENDIF
151000020204     C                   ENDIF
151100020204     C*
151200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
151300020204     C                   Z-ADD     vabIAS        wrkDec
151400020204     C                   IF        wrkDec > *zeros
151500020204     C                   IF        vabVAS = 'ITL'
151600020204     C                   EVAL      vabIAS = *zeros
151700020204     C                   ENDIF
151800020204     C                   ENDIF
151900020204     C*
152000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152100020204     C                   Z-ADD     vabCAS        wrkDec
152200020204     C                   IF        wrkDec > *zeros
152300020204     C                   IF        vabVCA = 'ITL'
152400020204     C                   EVAL      vabCAS = *zeros
152500020204     C                   ENDIF
152600020204     C                   ENDIF
152700020204     C*
152800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
152900020204     C                   Z-ADD     vabVMD        wrkDec
153000020204     C                   IF        wrkDec > *zeros
153100020204     C                   IF        vabVAD = 'ITL'
153200020204     C                   EVAL      vabVMD = *zeros
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C                   ENDSR
153700020204     C***
153800020204
153900010330
154000010330
154100010330     C*----------------------------------------------------*
154200010330     C*  CONTROLLO NUMERICITA' CAMPI
154300010330     C*----------------------------------------------------*
154400010330     C     CHKNUM        BEGSR
154500010330     C*
154600010606     C                   IF        PiDecChr = *blanks
154700030715     C                   EVAL      PiDecChr = CharNUM
154800010606     C                   ENDIF
154900010606     C*
155000010606     C                   CALL(e)   'ISNUMERIC'
155100010330     C                   PARM                    PiStr            30
155200010606     C                   PARM                    PiDecChr          1
155300010330     C                   PARM      *ZEROS        PiVal            30 9
155400010330     C                   PARM      '0'           PiInt             1
155500010330     C                   PARM      '0'           PiNum             1
155600010330     C                   IF        %error
155700010606     C                   EVAL      PiNum=*off
155800010330     C                   ENDIF
155900010330     C*
156000010330     C                   ENDSR
156100010330     C***
156200010330
156300010601
156400010601
156500010601
156600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156700010601     C     repfil        BEGSR
156800010601     C*
156900010601     C                   if        invfil = *zeros and
157000010601     C                             depfil > *zeros and
157100010629     C                             (vinflg = *blanks or
157200010629     C                              vinflg = *zeros)
157300010601     C
157400010601     C                   eval      invfil = depfil
157500010601     C                   endif
157600010601     C*
157700010601     C                   if        depfil <> invfil and
157800010601     C                             invfil > *zeros
157900010601     C                   eval      flgMulti = '1'
158000010601     C                   if        vinflg = *blanks
158100010601     C                   add       1             cntNonEl
158200010601     C                   endif
158300010601     C                   endif
158400010601     C*
158500010601     C                   if        vinflg = '2'
158600010601     C                   eval      flgStato = '2'
158700010601     C                   endif
158800010601     C*
158900010601     C                   ENDSR
159000010601     C***
159100010601
159200010601
159300010601
159400010330
159500040119      /TITLE Invio dei dati al punto operativo.
159600040119     C     invio         BEGSR
159700040119     C*
159800040119     C* 1° invio FIVAT
159900040119     C                   reset                   dscmz
160000070704     C                   move      winvcmz       cmzdst
160100040119     C                   eval      cmzfld = 'FIVATWWR'
160200040119     C                   eval      cmzmbd = vlrhdl
160300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160400040119     C***                if        prmfir = *blanks
160500040119     C                   eval      cmzfla = 'FIVAT00F'
160600040119     C                   eval      cmzmba = 'FIVAT00F'
160700040119     C***                else
160800040119     C***                eval      cmzfla = prmfir
160900040119     C***                eval      cmzmba = prmfir
161000040119     C***                endif
161100040119     C                   eval      cmznrr = *zeros
161200040119     C                   move      §ctrokvt      cmznrr
161300040119     C                   eval      cmzlba = vlrfl1
161400040119     C                   call(e)   'TIS711C'
161500040119     C                   parm                    dscmz
161600040119     C                   parm      *blanks       esito
161700040119     C                   if        %error
161800040119     C                             or cmzerr = '1'
161900040119     C                             or esito  = '1'
162000040119     C                   eval      wrkesito = '3'
162100040119     C                   else
162200040119     C*
162300040119     C* 2° invio FIVAB
162400040119     C                   reset                   dscmz
162500070704     C                   move      winvcmz       cmzdst
162600040119     C                   eval      cmzfld = vlrfou
162700040119     C                   eval      cmzmbd = vlrhdl
162800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162900040119     C***                if        prmfir = *blanks
163000040119     C                   eval      cmzfla = 'FIVAB00F'
163100040119     C                   eval      cmzmba = 'FIVAB00F'
163200040119     C***                else
163300040119     C***                eval      cmzfla = prmfir
163400040119     C***                eval      cmzmba = prmfir
163500040119     C***                endif
163600040119     C                   eval      cmznrr = *zeros
163700040119     C                   move      §ctrokvb      cmznrr
163800040119     C                   eval      cmzlba = vlrfl1
163900040119     C                   call(e)   'TIS711C'
164000040119     C                   parm                    dscmz
164100040119     C                   parm      *blanks       esito
164200040119     C                   if        %error
164300040119     C                             or cmzerr = '1'
164400040119     C                             or esito  = '1'
164500040119     C                   eval      wrkesito = '3'
164600040119     C                   endif
164700040119     C                   endif
164800040119     C*
164900040119     C                   ENDSR
165000040119     C***
165100010601
165200010601
165300010601
165400010601
165500010601      /TITLE Invio dei dati al punto operativo.
165600010601     C     opeini        BEGSR
165700010601     C*
165800010601     C* Inizializzo flag e contatori operativi
165900010601     C                   movel     '0'           flgGiro           1
166000010601     C                   movel     '0'           flgMulti          1
166100010601     C                   movel     '1'           flgStato          1
166200010615     C                   movel     '0'           flgOk             1
166300010601     C                   z-add     *zeros        cntNonEl         10 0
166400010601     C                   z-add     *zeros        depfil            3 0
166500010601     C                   z-add     *zeros        invfil            3 0
166600010601     C*
166700010601     C                   ENDSR
166800010601     C***
166900070326
167000070326
167100070326
167200070326
167300070326     C     *pssr         BEGSR
167400070329     C*
167500070329     C                   if        %open(tivin00r)
167600070329     C                   close     tivin00r
167700070329     C                   endif
167800070329     C                   if        %open(fivabwwr)
167900070329     C                   close     fivabwwr
168000070329     C                   endif
168100070329     C                   if        %open(fivatwwr)
168200070329     C                   close     fivatwwr
168300070329     C                   endif
168400070326     C*
168500070326     C* Effettuo la chiamata al CLLE preposto
168600070326     C                   call(e)   'TITVVTC'
168700070326     C                   parm                    parccm
168800070326     C                   parm                    parmbr
168900070326     C                   parm      '2'           paropz
169000070326     C*
169100070326     C                   eval      wrkesito = '2'
169200070404     C*
169300070404     C                   seton                                        LR
169400070326     C*
169500070326     C                   ENDSR     '*CANCL'
169600070326     C***
169700070326
169800070326
169900010330
170000010330
170100000613     C     *inzsr        BEGSR
170200990910     C*
170300990910     C     *entry        plist
170400990920     C                   parm                    tivlrds
170500990921     C                   parm      wrkesito      esito
170600000724     C                   parm                    prmlit
170700000710     C                   parm                    prmfir
170800010330     C*
170900010330     C* CALCOLA LA DATA CORRENTE
171000010330     C                   time                    wn14             14 0
171100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
171200010330     C                   z-add     wn8           g08dat
171300010330     C                   z-add     *zeros        g08inv
171400010330     C                   movel     '0'           g08err
171500010330     C                   call      'XSRDA8'
171600010330     C                   parm                    wlbda8
171700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
171800000613     C*
171900000613     C                   ENDSR
172000000613     C***
172100990908
172200040122     Otitv92p   E            riepilogo         2
172300990915     O                                              'Upload via Internet'
172400990915     O                                           +1 'Traduzione TIVIN00R -'
172500021025     O                                           +1 'FIVABWWR'
172600990915     O                                           +1 'Testate bolle'
172700990915     O          E            riepilogo   2
172800990915     O                       wrkdata
172900990915     O                       wrkora              +1
173000990915     O                       procname            +1
173100990915     O          E            riepilogo   2
173200990915     O                                              'Cliente..................:'
173300010605     O                       VABCCM        z     +1
173400990915     O          E            riepilogo   2
173500990920     O                                              'Riferimento Strategi.....:'
173600990920     O                       vlrhdl              +1
173700990915     O          E            riepilogo   2
173800990915     O                                              'Giusti...................:'
173900040119     O                       §CTROKVB      2   +  1
174000990915     O          E            riepilogo   2
174100990915     O                                              'Sbagliati e corretti.....:'
174200971022     O                       §CTRMO        2   +  1
174300990915     O          E            riepilogo   2
174400990915     O                                              'Sbagliati e scartati.....:'
174500971022     O                       §CTRNO        2   +  1
174600000613
174700040122     Otitv92ps  E            testdett          2
174800000613     O                                              'Upload via Internet'
174900000613     O                                           +1 'Traduzione TIVIN00R -'
175000021025     O                                           +1 'FIVABWWR'
175100000613     O                                           +1 'Testate bolle'
175200000616     O          E            testdett    3
175300000613     O                                           +2 'N° rec'
175400000613     O                                           +3 'Anteprima contenuto'
175500000616     O          E            rigadett    2
175600000613     O                       rrnum               +2
175700000621     O                       recko               +3
175800000616     O          E            findett     2
175900000613     O                       wrkdata
176000000613     O                       wrkora              +1
176100000613     O                       procname            +1
176200000616     O          E            findett     2
176300000613     O                                              'Cliente..................:'
176400010605     O                       VABCCM        z     +1
176500000616     O          E            findett     2
176600000613     O                                              'Riferimento Strategi.....:'
176700000613     O                       vlrhdl              +1
176800000616     O          E            findett     2
176900000613     O                                              'Giusti...................:'
177000040119     O                       §CTROKVB      2   +  1
177100000616     O          E            findett     2
177200000613     O                                              'Sbagliati e corretti.....:'
177300000613     O                       §CTRMO        2   +  1
177400000616     O          E            findett     2
177500000613     O                                              'Sbagliati e scartati.....:'
177600000613     O                       §CTRNO        2   +  1
177700000613** CMD - COMANDI CL
177800040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
177900040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
178000040122DLTOVR FILE(TITV92P TITV92PS) LVL(*)
178100010606
178200000613
178300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178400040510
178500040122
178600040113
