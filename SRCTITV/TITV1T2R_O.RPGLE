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
028400091204     C* Forzatura x distinzione "univoca" delimitatore campo => serie centrali
028500091204     C                   dow       %scan(
028600091204     C                                   %trim(CharTXT)+
028700091204     C                                   %trim(CharCSV)+
028800091204     C                                   %trim(CharTXT)
028900091204     C                                   :vindta) > *zeros
029000091204     C                   eval      vindta = %replace(
029100091204     C                                      %trim(CharTXT)+
029200091204     C                                      '§'+
029300091204     C                                      %trim(CharTXT)
029400091204     C                                      :vindta:
029500091204     C                                      %scan(
029600091204     C                                      %trim(CharTXT)+
029700091204     C                                      %trim(CharCSV)+
029800091204     C                                      %trim(CharTXT)
029900091204     C                                      :vindta))
030000091204     C                   enddo
030100091204     C*
030200091204     C* Forzatura x distinzione "univoca" delimitatore campo => serie iniziali
030300091204     C                   dow       %scan(
030400091204     C                                   %trim(CharTXT)+
030500091204     C                                   %trim(CharCSV)
030600091204     C                                   :vindta) > *zeros
030700091204     C                   eval      vindta = %replace(
030800091204     C                                      %trim(CharTXT)+
030900091204     C                                      '§'
031000091204     C                                      :vindta:
031100091204     C                                      %scan(
031200091204     C                                      %trim(CharTXT)+
031300091204     C                                      %trim(CharCSV)
031400091204     C                                      :vindta))
031500091204     C                   enddo
031600091204     C*
031700091204     C* Forzatura x distinzione "univoca" delimitatore campo => serie finali
031800091204     C                   dow       %scan(
031900091204     C                                   %trim(CharCSV)+
032000091204     C                                   %trim(CharTXT)
032100091204     C                                   :vindta) > *zeros
032200091204     C                   eval      vindta = %replace(
032300091204     C                                      '§'+
032400091204     C                                      %trim(CharTXT)
032500091204     C                                      :vindta:
032600091204     C                                      %scan(
032700091204     C                                      %trim(CharCSV)+
032800091204     C                                      %trim(CharTXT)
032900091204     C                                      :vindta))
033000091204     C                   enddo
033100070925     C*
033200010330     C                   exsr      inzvar
033300010605     C                   exsr      impvab
033400010601     C*
033500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
033600010605     C                   eval      depfil = VABLNP
033700010601     C                   exsr      repfil
033800010601     C                   if        depfil = invfil
033900021025     C                   if        vlrpoi = 999
034000030822     C                   move(P)   invfil        VABFGS
034100021025     C                   else
034200030822     C                   move(P)   vlrpoi        VABFGS
034300021025     C                   endif
034400010601     C*
034500010601     C                   exsr      PREELA
034600010601     C*
034700010604     C* Ebbene...
034800010604     C*
034900040119     C  N31              add       1             §CTROKVB
035000040119     C   32              add       1             §CTRMO
035100040119     C   31              add       1             §CTRNO
035200030822     C                   if        wGiro = 2
035300040510     C* Scrivo sempre il VAT
035400040510     C  N31              exsr      wrivat
035500040510     C* Scrivo il VAB solo a rottura d spedizione
035600040119     C                   if        depspe =  *zeros OR
035700040119     C                             depspe <> vabNSP
035800040119     C  N31              exsr      wrivab
035900040119     C  N31              eval      depspe = vabNSP
036000040119     C                   endif
036100040119     C                   endif
036200010604     C*
036300010604     C                   if        *in31 = *off and
036400010604     C                             *in32 = *off
036500010604     C                   eval      vinflg = '1'
036600010604     C                   else
036700010604     C                   eval      recko = vindta
036800010604     C                   except    rigadett
036900010604     C                   eval      vinflg = '2'
037000010604     C                   endif
037100010604     C                   endif
037200010601     C*
037300010604     C                   endif
037400010604     C*
037500010330     C                   else
037600010330     C                   eval      vinflg = '1'
037700010330     C                   endif
037800010601     C*
037900010601     C  N70              update    tivin000
038000010330     C*
038100030822     C  N70              enddo
038200040119     C*
038300040119     C                   endif                                                  (endif - chkcall)
038400010601     C*
038500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
038600010601     C                   if        cntNonEl = *zeros or
038700010601     C                             flgMulti = '0'
038800010330     C* Se non ci sono record con errori ...
038900010601     C                   if        §ctrno = 0 and
039000010604     C                             §ctrmo = 0 and
039100010601     C                             flgStato <> '2'
039200010330     C* ... restituisco esito OK.
039300010330     C                   eval      wrkesito = '0'
039400010330     C                   else
039500040119     C                   if        §ctrokvb > 0
039600010330     C                   eval      wrkesito = '1'
039700010330     C                   else
039800010615     C                   if        flgOk = '0'
039900010615     C                   eval      wrkesito = '2'
040000010615     C                   else
040100010615     C                   eval      wrkesito = '6'
040200010615     C                   endif
040300010330     C                   endif
040400010330     C                   endif
040500010601     C                   else
040600010601     C                   eval      wrkesito = '9'
040700010601     C                   endif
040800010330     C*
040900010330     C                   if        %open(tivin00r)
041000010330     C                   close     tivin00r
041100010330     C                   endif
041200021025     C                   if        %open(fivabwwr)
041300021025     C                   close     fivabwwr
041400010330     C                   endif
041500040512     C                   if        %open(fivatwwr)
041600040512     C                   close     fivatwwr
041700040512     C                   endif
041800010601     C*
041900010601     C                   if        vlrpoi <> 999
042000010601     C                   eval      invfil = vlrpoi
042100010601     C                   endif
042200010330     C*
042300040119     C                   if        §ctrokvb > 0
042400010601     C                             and invfil > *zeros
042500010330     C                   exsr      invio
042600010330     C                   endif
042700010601     C*
042800010618     C                   if        flgGiro = '1'
042900081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
043000010618     C                   endif
043100010330     C*
043200010330     C                   ENDSR
043300010330     C***
043400040119
043500040119
043600040119
043700040119     C*----------------------------------------------------*
043800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
043900040119     C*----------------------------------------------------*
044000040510     C     WRIVAB        BEGSR
044100040119     C*
044200060331     C* Gestisco l'eventuale rottura x numero spedizione
044300070823     C                   if        VABNSP <> depspe
044400070301     C                   if        wVATNOT_E <> *blanks
044500080125     C                   if        VABCTM = *blanks
044600080125     C                   eval      VABCTM = '7Q'
044700080125     C                   endif
044800070301     C                   endif
044900040119     C                   write     fivab000                                     => scarico il VAB
045000060331     C                   endif
045100040119     C*
045200040119     C                   ENDSR
045300040119
045400040119
045500040119
045600040119     C*----------------------------------------------------*
045700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
045800040119     C*----------------------------------------------------*
045900040510     C     WRIVAT        BEGSR
046000060512     C*
046100060512     C* Inizializzo i campi chiave
046200060512     C                   eval      VATFGS = VABFGS
046300060512     C                   eval      VATAAS = VABAAS
046400060512     C                   eval      VATCCM = VABCCM
046500070823     C                   eval      VATNRS = VABNRS
046600060512     C                   eval      VATNSP = VABNSP
046700060512     C                   eval      VATLNP = VABLNP
046800060331     C*
046900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
047000060331     C                   if        wVATNOT_A <> *blanks
047100060331     C                   eval      VATNOT = wVATNOT_A
047200060331     C                   eval      VATTRC = 'A'
047300060331     C                   write     FIVAT000
047400060331     C                   add       1             §CTROKVT
047500060331     C                   endif
047600060331     C*
047700060331     C                   if        wVATNOT_B <> *blanks
047800060331     C                   eval      VATNOT = wVATNOT_B
047900060331     C                   eval      VATTRC = 'B'
048000060331     C                   write     FIVAT000
048100060331     C                   add       1             §CTROKVT
048200060331     C                   endif
048300060331     C*
048400080625     C                   if        wVATNOT_E <> *blanks
048500060331     C                   eval      VATNOT = wVATNOT_E
048600060331     C                   eval      VATTRC = 'E'
048700080716     C                   write     FIVAT000
048800080716     C                   add       1             §CTROKVT
048900080625     C                   endif
049000040119     C*
049100040119     C                   ENDSR
049200040119
049300040119
049400040119
049500040119     C*----------------------------------------------------*
049600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049700040119     C*----------------------------------------------------*
049800040119     C     PREVAT        BEGSR
049900040119     C*
050000040119     C* Compongo il nome del membro da dare al FIVATWWR
050100040119     C                   eval      parmbr = vlrhdl
050200040119     C                   movel     'M'           parmbr
050300040510     C                   eval      parccm = vlrksc
050400040119     C                   eval      paropz = '1'
050500040119     C* Effettuo la chiamata al CLLE preposto
050600040119     C                   call(e)   'TITVVTC'
050700040119     C                   parm                    parccm
050800040119     C                   parm                    parmbr
050900040119     C                   parm                    paropz
051000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051100040119     C                   if        %error
051200040119     C                   movel     '1'           chkcall
051300040119     C                   else
051400040119     C                   movel     '0'           chkcall
051500040119     C                   endif
051600040119     C*
051700040119     C                   ENDSR
051800010601
051900010601
052000010601
052100010330     C*----------------------------------------------------*
052200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052300010330     C*----------------------------------------------------*
052400040119     C     INZVAR        BEGSR
052500010330     C*
052600030822     C* Inizializzo variabili di wrk
052700020204     C                   Z-ADD     *zeros        Num5_0            5 0
052800060331     C                   MOVEL     *blanks       wVATNOT_A
052900060331     C                   MOVEL     *blanks       wVATNOT_B
053000060331     C                   MOVEL     *blanks       wVATNOT_E
053100090220     C*
053200080617     C                   EVAL      wFlgCAS = *blanks
053300030822     C*
053400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053500030822     C                   CLEAR                   FIVAB000
053600040119     C                   CLEAR                   FIVAT000
053700030822     C                   CLEAR                   SkSplitCSV
053800030822     C                   EVAL      i = 1
053900030822     C                   EVAL      posDa = *zeros
054000030822     C                   EVAL      posA  = *zeros
054100030822     C*
054200030822     C* Reimposto i valori di default
054300030822     C                   EXSR      DEFCAM
054400030822     C*
054500010330     C                   ENDSR
054600010330     C*----------------------------------------------------*
054700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054800010330     C*----------------------------------------------------*
054900010330     C     DEFCAM        BEGSR
055000030822     C*
055100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055200030715     C* e delimitatore testo.
055300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055600030822     C*
055700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
055900030715     C                   SETON                                        50
056000030715     C                   ELSE
056100030715     C                   SETOFF                                       50
056200030715     C                   ENDIF
056300090421     C*
056400090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
056500090421     C                   EVAL      VABCBO = '1'
056600090421     C                   EVAL      VABLNP = vlrpoi
056700090421     C                   EVAL      VABNSP = rrnum
056800030822     C*
056900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057000030822     C                   EVAL      posDaDft = 1
057100030822     C                   EVAL      posADft  = 0
057200030822     C                   EVAL      wGiroDft = 0
057300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057400030822     C                             posDaDft > 0
057500030822     C*
057600030822     C* Gestisco il 1° giro
057700030822     C                   IF        wGiroDft = 0
057800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058000030822     C* Incremento il contatore dei "giri"
058100030822     C                   EVAL      wGiroDft = 1
058200030822     C                   ELSE
058300030822     C                   EVAL      posDaDft = posADft
058400030822     C                   ENDIF
058500030822     C* Eseguo lo scan x trovare la fine del campo corrente
058600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058700030822     C*
058800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059000030822     C                   IF        posDaDft > 0 AND
059100030822     C                             posADft  > 0
059200030822     C* NCL
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'NCL'
059700030822     C                   EVAL      PiStr=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   EXSR      CHKNUM
060100030822     C                   IF        PiInt=*on
060200030822     C                   Z-ADD     PiVal         VABNCL
060300030822     C                   ENDIF
060400030822     C                   ENDIF
060500030822     C* CCM
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'CCM'
061000030822     C                   EVAL      PiStr=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   EXSR      CHKNUM
061400030822     C                   IF        PiInt=*on
061500030822     C                   Z-ADD     PiVal         VABCCM
061600030822     C                   ENDIF
061700030822     C                   ENDIF
061800030822     C* LNP
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'LNP'
062300030822     C                   EVAL      PiStr=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   EXSR      CHKNUM
062700030822     C                   IF        PiInt=*on
062800030822     C                   Z-ADD     PiVal         VABLNP
062900030822     C                   ENDIF
063000030822     C                   ENDIF
063100030822     C* NRS
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'NRS'
063600030822     C                   EVAL      PiStr=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   EXSR      CHKNUM
064000030822     C                   IF        PiInt=*on
064100030822     C                   Z-ADD     PiVal         VABNRS
064200030822     C                   ENDIF
064300030822     C                   ENDIF
064400030822     C* CTR
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'CTR'
064900030822     C                   EVAL      PiStr=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   EXSR      CHKNUM
065300030822     C                   IF        PiInt=*on
065400030822     C                   Z-ADD     PiVal         VABCTR
065500030822     C                   ENDIF
065600030822     C                   ENDIF
065700030822     C* PKB
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'PKB'
066200030822     C                   EVAL      PiStr=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   EXSR      CHKNUM
066600030822     C                   IF        PiNum=*on
066700030822     C                   Z-ADD     PiVal         VABPKB
066800030822     C                   ENDIF
066900030822     C                   ENDIF
067000030822     C* VLB
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'VLB'
067500030822     C                   EVAL      PiStr=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   EXSR      CHKNUM
067900030822     C                   IF        PiNum=*on
068000030822     C                   Z-ADD     PiVal         VABVLB
068100030822     C                   ENDIF
068200030822     C                   ENDIF
068300030822     C* QFT
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'QFT'
068800030822     C                   EVAL      PiStr=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   EXSR      CHKNUM
069200030822     C                   IF        PiNum=*on
069300030822     C                   Z-ADD     PiVal         VABQFT
069400030822     C                   ENDIF
069500030822     C                   ENDIF
069600030822     C* CBO
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'CBO'
070100030822     C                   EVAL      VABCBO=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* TSP
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'TSP'
071000030822     C                   EVAL      VABTSP=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* VAS
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'VAS'
071900030822     C                   EVAL      VABVAS=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* VCA
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'VCA'
072800030822     C                   EVAL      VABVCA=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200070925     C* TIC
073300070925     C                   IF        %subst(
073400070925     C                             %subst(vlrppt:posDaDft+1:
073500070925     C                             posADft-posDaDft-1):1:3)
073600070925     C                             = 'TIC'
073700070925     C                   EVAL      VABTIC=%trim(%subst(
073800070925     C                             %subst(vlrppt:posDaDft+1:
073900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000070925     C                   ENDIF
074100030822     C* GCA
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'GCA'
074600030822     C                   EVAL      VABGCA=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* CTM
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'CTM'
075500030822     C                   EVAL      VABCTM=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* FFD
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'FFD'
076400030822     C                   EVAL      VABFFD=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* VAD
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'VAD'
077300030822     C                   EVAL      VABVAD=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* GMA
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'GMA'
078200030822     C                   EVAL      VABGMA=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* GGA
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'GGA'
079100030822     C                   EVAL      VABGGA=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500030822     C* GVA
079600030822     C                   IF        %subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):1:3)
079900030822     C                             = 'GVA'
080000030822     C                   EVAL      VABGVA=%trim(%subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300030822     C                   ENDIF
080400030822     C* TC1
080500030822     C                   IF        %subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):1:3)
080800030822     C                             = 'TC1'
080900030822     C                   EVAL      VABTC1=%trim(%subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081200030822     C                   ENDIF
081300030822     C* TC2
081400030822     C                   IF        %subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):1:3)
081700030822     C                             = 'TC2'
081800030822     C                   EVAL      VABTC2=%trim(%subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082100030822     C                   ENDIF
082200040714     C* VATTRC
082300040714     C                   IF        %subst(
082400040714     C                             %subst(vlrppt:posDaDft+1:
082500040714     C                             posADft-posDaDft-1):1:3)
082600040714     C                             = 'TRC'
082700040714     C                   EVAL      VATTRC=%trim(%subst(
082800040714     C                             %subst(vlrppt:posDaDft+1:
082900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083000040714     C                   ENDIF
083100030822     C* ...
083200030822     C                   ENDIF
083300030822     C                   ENDDO
083400020204     C*
083500010330     C                   ENDSR
083600010607     C*----------------------------------------------------*
083700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083800010607     C*----------------------------------------------------*
083900010607     C     IMPVAB        BEGSR
084000010607     C*
084100010607     C                   SETOFF                                       3132
084200030515     C*
084300010607     C                   MOVEL     datcor        VABAAS
084400010607     C                   MOVE      datcor        VABMGS
084500010607     C*
084600030715     C**********
084700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084800030715     C**********
084900030715     C                   IF        wGiro = *zeros
085000080716     C*
085100080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
085200080716     C                   EVAL      depvinDTA = vinDTA
085300040113     C*
085400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085600090716     C                             %trim(intcol(3))+%trim(intcol(4))+
085700091204     C                             %trim(intcol(5))+%trim(intcol(6))+
085800091204     C                             %trim(intcol(7))<>*blanks
085900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086000090716     C                                      %trim(intcol(3))+%trim(intcol(4))+
086100091204     C                                      %trim(intcol(5))+%trim(intcol(6))+
086200091204     C                                      %trim(intcol(7))
086300091204 xxx C***  ';':CharCSV   XLATE     vindta        vindta
086400040510     C                   ENDIF
086500080626     C*
086600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086700031201     C     minu:maiu     XLATE     vindta        vindta
086800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086900030715     C                   DOW       posDa <= %len(%trim(vindta))
087000030715     C*
087100030715     C* Gestisco il 1° campo
087200030715     C                   IF        i = 1
087300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087400030715     C                   EVAL      posDa = 1
087500030715     C* Eseguo lo scan x trovare la fine del primo campo
087600091204     C                   EVAL      posA  = %scan('§':vindta:posDa+1)
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C* X i campi successivi al 1°
088100030715     C                   ELSE
088200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088300030715     C                   EVAL      posDa = posA + 1
088400030715     C* Eseguo lo scan x trovare la fine del campo corrente
088500091204     C                   EVAL      posA  = %scan('§':vindta:posDa+1)
088600030715     C* Gestisco l'ultimo campo
088700030715     C                   IF        posA = *zeros
088800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C                   LEAVE
089300030715     C                   ELSE
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C                   ENDIF
089800030715     C                   ENDIF
089900030715     C* Incremento il contatore d campo
090000030715     C                   EVAL      i = i +1
090100030715     C                   ENDDO
090200030715     C*
090300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090400030715     C* delimitazione testo
090500030715     C                   EVAL      i = 1
090600030715     C                   DOW       i < %elem(SkSplitFLD)
090700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090900030715     C                   EVAL      i = i + 1
091000030715     C                   ENDDO
091100030715     C                   EVAL      wGiro = 1
091200080716     C* Ripristino la 1° riga originale
091300080716     C                   EVAL      vinDTA = depvinDTA
091400030715     C                   ELSE
091500030715     C                   EVAL      wGiro = 2
091600031204     C**********
091700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091800031204     C* d separatore campo
091900031204     C**********
092000031204     C                   EVAL      vindta = %trim(vindta)
092100091204     C                   DOW       %subst(vindta:1:1) = '§'                     * all'inizio
092200031204     C                   EVAL      vindta = %subst(vindta:2)
092300031204     C                   ENDDO
092400031204     C*
092500031204     C                   Z-ADD     *zeros        lunghInput        4 0
092600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092700091204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = '§'
092800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093000031204     C                   ENDDO
093100030715     C**********
093200030715     C* Effettuo lo split del campo dati d input
093300030715     C**********
093400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093500030715     C                   DOW       posDa <= %len(%trim(vindta))
093600030715     C*
093700030715     C* Gestisco il 1° campo
093800030715     C                   IF        i = 1
093900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094000030715     C                   EVAL      posDa = 1
094100030715     C* Eseguo lo scan x trovare la fine del primo campo
094200091204     C                   EVAL      posA  = %scan('§':vindta:posDa+1)
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C* X i campi successivi al 1°
094700030715     C                   ELSE
094800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094900030715     C                   EVAL      posDa = posA + 1
095000031204     C* Verifico che nn vi sia il campo nn valorizzato
095100091204     C                   IF        %subst(vindta:posDa:1) = '§'
095200031204     C* Se campo nn valorizzato skippo al prossimo
095300031204     C                   EVAL      SkSplitCSV(i) = *blanks
095400031204     C                   EVAL      posA  = posA + 1
095500031204     C                   ELSE
095600030715     C* Eseguo lo scan x trovare la fine del campo corrente
095700091204     C                   EVAL      posA  = %scan('§':vindta:posDa+1)
095800030715     C* Gestisco l'ultimo campo
095900030715     C                   IF        posA = *zeros
096000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
096100030715     C* A questo punto "estraggo" il campo corrente
096200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096300030715     C                                                    (posA-posDa))
096400030715     C                   LEAVE
096500030715     C                   ELSE
096600030715     C* A questo punto "estraggo" il campo corrente
096700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096800030715     C                                                    (posA-posDa))
096900030715     C                   ENDIF
097000031204     C                   ENDIF
097100030715     C                   ENDIF
097200030715     C* Incremento il contatore d campo
097300030715     C                   EVAL      i = i +1
097400030715     C                   ENDDO
097500030715     C*
097600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097700030715     C* delimitazione testo
097800030715     C                   EVAL      i = 1
097900030715     C                   DOW       i < %elem(SkSplitCSV)
098000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098200030715     C                   EVAL      i = i + 1
098300030715     C                   ENDDO
098400030715     C*
098500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098600030715     C                   EVAL      i = 1
098700030715     C                   DOW       i < %elem(SkSplitFLD)
098800030715     C*
098900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099100030715     C                             SkSplitCSV(i) <> *blanks
099200030721     C*** ATB
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099400030721     C                   EVAL      VABATB = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** RSD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100100030715     C* ==
100200030715     C                   ENDIF
100300030721     C*** RD2
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030715     C*** IND
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100900030715     C                   EVAL      VABIND = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** LOD
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** PRD
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030721     C*** NZD
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030715     C*** RMA
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** NOT
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** NT2
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** VCA
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** VAS
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** FFD
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030721     C*** GC1
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GC2
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TSP
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105800030721     C                   ENDIF
105900040510     C*** CBO
106000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106200040510     C                   ENDIF
106300030721     C*** NAS
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TIC
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GCA
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** XCO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** CTM
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** TCR
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** CTS
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** FTM
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** VAD
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GMA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GGA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** GVA
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** TC1
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** TC2
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** SCL
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** RMO
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** NMO
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*
113200030721     C* Reperisco quindi i campi numerici...
113300040510     C*** NSP
113400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113500040510     C                   EVAL      PiStr=SkSplitCSV(i)
113600040510     C                   EXSR      CHKNUM
113700040510     C                   IF        PiInt=*on
113800040510     C                   Z-ADD     PiVal         VABNSP
113900040510     C                   ELSE
114000040510     C                   SETON                                        31
114100040510     C                   EVAL      VABNSP = *zeros
114200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040510     C                             + ' ' + 'VABNSP'
114400040510     C                   ENDIF
114500040510     C                   ENDIF
114600071106     C*** RMN
114700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114800071106     C                   EVAL      PiStr=SkSplitCSV(i)
114900071106     C                   EXSR      CHKNUM
115000071106     C                   IF        PiInt=*on
115100071106     C                   Z-ADD     PiVal         VABRMN
115200071106     C                   ELSE
115300071106     C                   SETON                                        32
115400071106     C                   EVAL      VABRMN = *zeros
115500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115600071106     C                             + ' ' + 'VABRMN'
115700071106     C                   ENDIF
115800071106     C                   ENDIF
115900030721     C*** CAD
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on
116400030721     C                   Z-ADD     PiVal         Num5_0
116500030721     C                   MOVEL(p)  Num5_0        VABCAD
116600030721     C                   ELSE
116700090603     C                   EVAL      VABCAD = SkSplitCSV(i)
116800090603     C                   SETON                                        32
116900090603     C                   EVAL      VABCAD = *zeros
117000090603     C                   EVAL      vinmsg = %trimr(vinmsg)
117100090603     C                             + ' ' + 'VABCAD'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** NCL
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on
117900030721     C                   Z-ADD     PiVal         VABNCL
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      VABNCL = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400040122     C                             + ' ' + 'VABNCL'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** PKB
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABPKB
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      VABPKB = *zeros
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABPKB'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** CAS
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120200080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
120300080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
120400081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
120500090716     C                             %trim(SkSplitCSV(i)) <> '.0'     AND
120600090716     C                             %trim(SkSplitCSV(i)) <> '.00'    AND
120700090716     C                             %trim(SkSplitCSV(i)) <> ',0'     AND
120800090716     C                             %trim(SkSplitCSV(i)) <> ',00'    AND
120900090716     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
121000090716     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
121100090716     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
121200081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
121300080125     C                   EVAL      wFlgCAS = '1'
121400080125     C                   ENDIF
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiNum=*on
121800030721     C                   Z-ADD     PiVal         VABCAS
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABCAS = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCAS'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** CCM
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on      AND
123100030721     C                             PiVal<=9999999 AND
123200030721     C                             PiVal>*zeros
123300030721     C                   Z-ADD     PiVal         VABCCM
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      VABCCM = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800040510     C                             + ' ' + 'VABCCM'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** LNP
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on    AND
124600030721     C                             PiVal<=999   AND
124700030721     C                             PiVal>*zeros
124800030721     C                   Z-ADD     PiVal         VABLNP
124900030721     C                   ELSE
125000040715     C                   SETON                                        32
125100030721     C                   EVAL      VABLNP = *zeros
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040510     C                             + ' ' + 'VABLNP'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** NRS
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on AND
126100030721     C                             PiVal<=99
126200030721     C                   Z-ADD     PiVal         VABNRS
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600030721     C                             + ' ' + 'VABNRS'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900030721     C*** LNA
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiInt=*on    AND
127400030721     C                             PiVal<=999   AND
127500030721     C                             PiVal>*zeros
127600030721     C                   Z-ADD     PiVal         VABLNA
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      VABLNA = *zeros
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABLNA'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** CTR
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on  AND
128900030721     C                             PiVal<=999
129000030721     C                   Z-ADD     PiVal         VABCTR
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABCTR'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** IAS
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiNum=*on
130200030721     C                   Z-ADD     PiVal         VABIAS
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABIAS'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** VLB
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiNum=*on
131400030721     C                   Z-ADD     PiVal         VABVLB
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABVLB'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** QFT
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiNum=*on
132600030721     C                   Z-ADD     PiVal         VABQFT
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABQFT'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** NCD
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on      AND
133800030721     C                             PiVal<=9999999
133900030721     C                   Z-ADD     PiVal         VABNCD
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABNCD'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** NCA
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on      AND
135100030721     C                             PiVal<=9999999
135200030721     C                   Z-ADD     PiVal         VABNCA
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABNCA'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** ZNC
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on AND
136400030721     C                             PiVal<=99
136500030721     C                   Z-ADD     PiVal         VABZNC
136600030721     C                   ELSE
136700030721     C                   SETON                                        32
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABZNC'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200030721     C*** DCR
137300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiInt=*on       AND
137700030721     C                             PiVal<=99999999
137800030721     C                   Z-ADD     PiVal         VABDCR
137900030721     C                   ELSE
138000030721     C                   SETON                                        32
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200030721     C                             + ' ' + 'VABDCR'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500030721     C*** HCR
138600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138700030721     C                   EVAL      PiStr=SkSplitCSV(i)
138800030721     C                   EXSR      CHKNUM
138900030721     C                   IF        PiInt=*on   AND
139000030721     C                             PiVal<=9999
139100030721     C                   Z-ADD     PiVal         VABHCR
139200030721     C                   ELSE
139300030721     C                   SETON                                        32
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500030721     C                             + ' ' + 'VABHCR'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800030721     C*** VMD
139900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140000030721     C                   EVAL      PiStr=SkSplitCSV(i)
140100030721     C                   EXSR      CHKNUM
140200030721     C                   IF        PiNum=*on
140300030721     C                   Z-ADD     PiVal         VABVMD
140400030721     C                   ELSE
140500030721     C                   SETON                                        32
140600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140700030721     C                             + ' ' + 'VABVMD'
140800030721     C                   ENDIF
140900030721     C                   ENDIF
141000030721     C*** ANT
141100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141200030721     C                   EVAL      PiStr=SkSplitCSV(i)
141300030721     C                   EXSR      CHKNUM
141400030721     C                   IF        PiInt=*on        AND
141500030721     C                             PiVal<=999999999
141600030721     C                   Z-ADD     PiVal         VABANT
141700030721     C                   ELSE
141800030721     C                   SETON                                        32
141900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142000030721     C                             + ' ' + 'VABANT'
142100030721     C                   ENDIF
142200030721     C                   ENDIF
142300030721     C*** CMO
142400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142500030721     C                   EVAL      PiStr=SkSplitCSV(i)
142600030721     C                   EXSR      CHKNUM
142700030721     C                   IF        PiInt=*on
142800030721     C                   Z-ADD     PiVal         Num5_0
142900030721     C                   MOVEL(p)  Num5_0        VABCMO
143000030721     C                   ELSE
143100030721     C                   SETON                                        32
143200030721     C                   EVAL      VABCMO = *zeros
143300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143400030721     C                             + ' ' + 'VABCMO'
143500030721     C                   ENDIF
143600030721     C                   ENDIF
143700040510     C*
143800040510     C* Reperisco i campi del FIVAT
143900040510     C*** VATNOT
144000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
144200040510     C                   ENDIF
144300040510     C*** VATTRC
144400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144600040510     C                   ENDIF
144700060331     C*** VATNOT_A
144800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145000060331     C                   ENDIF
145100060331     C*** VATNOT_B
145200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145400060331     C                   ENDIF
145500060331     C*** VATNOT_E
145600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145800060331     C                   ENDIF
145900070823     C*
146000071029     C***  ===>  Gestione campi "particolari" dentro ciclo
146100090421     C*
146200070925     C*
146300071029     C***  <===  -----------------------------------------
146400040510     C*
146500030715     C                   ENDIF
146600030715     C                   EVAL      i = i + 1
146700030715     C                   ENDDO
146800070925     C*
146900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147000080125     C*
147100070925     C*
147200071029     C***  <===  -----------------------------------------
147300070925     C*
147400070925     C* Considerazioni finali su CBO/CAS
147500080125     C                   IF        wFlgCAS = '1'
147600070925     C                   IF        VABCBO = '1'
147700070925     C                   EVAL      VABCBO = '4'
147800070925     C                   ENDIF
147900070925     C                   IF        VABCBO = '2'
148000070925     C                   EVAL      VABCBO = '6'
148100070925     C                   ENDIF
148200070925     C                   ENDIF
148300070925     C***  <===  ----------------------------
148400040510     C*
148500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148600040510     C                   IF        VABNSP = *zeros
148700040510     C                   SETON                                        31
148800040510     C                   ENDIF
148900040715     C*
149000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149100040715     C                   IF        VABLNP = *zeros
149200040715     C                   SETON                                        31
149300040715     C                   ENDIF
149400040510     C*
149500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149600040510     C                   IF        VABRMN = *zeros
149700040510     C                   Z-ADD     VABNSP        VABRMN
149800040510     C                   ENDIF
149900040510     C*
150000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150100040510     C                   IF        VABRMA = *blanks
150200040714     C                   MOVEL     VABRMN        VABRMA
150300040510     C                   ENDIF
150400040128     C*
150500040510     C* Se provincia nn valorizzata la reperisco
150600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150700040510     C                   IF        VABPRD = *blanks
150800040128     C                   CLEAR                   TISI95DS
150900040128     C                   EVAL      I95TCN = '3'
151000040128     C                   Z-ADD     datcor        I95DAT
151100040128     C                   EVAL      I95NAR = VABNZD
151200040128     C                   EVAL      I95CAP = VABCAD
151300040128     C                   EVAL      I95LOC = VABLOD
151400040128     C                   CALL      'TISI95R'
151500040128     C                   PARM                    TISI95DS
151600040128     C                   EVAL      VABPRD = O95PRV
151700040510     C                   ENDIF
151800020204     C*
151900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152000020204     C                   EXSR      CHKIMPDIV
152100030715     C                   ENDIF
152200020204     C*
152300010607     C                   ENDSR
152400010607     C*----------------------------------------------------*
152500010601
152600020204
152700020204     C*----------------------------------------------------*
152800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152900020204     C*----------------------------------------------------*
153000020204     C     CHKIMPDIV     BEGSR
153100020204     C*
153200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153300020204     C                   Z-ADD     *zeros        wrkDec            9 9
153400020204     C*
153500020204     C* Come prima cosa effettuo considerazioni sulla divisa
153600020204     C                   IF        vabIAS > *zeros
153700020204     C                   IF        vabVAS <> 'EUR'
153800020204     C                   EVAL      vabVAS =  'ITL'
153900020204     C                   ENDIF
154000020204     C                   ENDIF
154100020204     C*
154200020204     C                   IF        vabCAS > *zeros
154300020204     C                   IF        vabVCA <> 'EUR'
154400020204     C                   EVAL      vabVCA =  'ITL'
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C                   IF        vabVMD > *zeros
154900020204     C                   IF        vabVAD <> 'EUR'
155000020204     C                   EVAL      vabVAD =  'ITL'
155100020204     C                   ENDIF
155200020204     C                   ENDIF
155300020204     C*
155400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155500020204     C                   Z-ADD     vabIAS        wrkDec
155600020204     C                   IF        wrkDec > *zeros
155700020204     C                   IF        vabVAS = 'ITL'
155800020204     C                   EVAL      vabIAS = *zeros
155900020204     C                   ENDIF
156000020204     C                   ENDIF
156100020204     C*
156200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156300020204     C                   Z-ADD     vabCAS        wrkDec
156400020204     C                   IF        wrkDec > *zeros
156500020204     C                   IF        vabVCA = 'ITL'
156600020204     C                   EVAL      vabCAS = *zeros
156700020204     C                   ENDIF
156800020204     C                   ENDIF
156900020204     C*
157000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157100020204     C                   Z-ADD     vabVMD        wrkDec
157200020204     C                   IF        wrkDec > *zeros
157300020204     C                   IF        vabVAD = 'ITL'
157400020204     C                   EVAL      vabVMD = *zeros
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C                   ENDSR
157900020204     C***
158000080617
158100080617
158200080617
158300080617     C*----------------------------------------------------*
158400080617     C*  CONTROLLO NUMERICITA' CAMPI
158500080617     C*----------------------------------------------------*
158600080617     C     CHKNUM        BEGSR
158700080617     C*
158800080617     C                   IF        PiDecChr = *blanks
158900080617     C                   EVAL      PiDecChr = CharNUM
159000080617     C                   ENDIF
159100080617     C*
159200080617     C                   callp     UBISNUM_Check(PiStr
159300080617     C                                          :PiDecChr
159400080617     C                                          :PiVal
159500080617     C                                          :PiNum
159600080617     C                                          :PiInt)
159700080617     C*
159800080617     C                   ENDSR
159900080617     C***
160000010330
160100010601
160200010601
160300010601
160400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160500010601     C     repfil        BEGSR
160600010601     C*
160700010601     C                   if        invfil = *zeros and
160800010601     C                             depfil > *zeros and
160900010629     C                             (vinflg = *blanks or
161000010629     C                              vinflg = *zeros)
161100010601     C
161200010601     C                   eval      invfil = depfil
161300010601     C                   endif
161400010601     C*
161500010601     C                   if        depfil <> invfil and
161600010601     C                             invfil > *zeros
161700010601     C                   eval      flgMulti = '1'
161800010601     C                   if        vinflg = *blanks
161900010601     C                   add       1             cntNonEl
162000010601     C                   endif
162100010601     C                   endif
162200010601     C*
162300010601     C                   if        vinflg = '2'
162400010601     C                   eval      flgStato = '2'
162500010601     C                   endif
162600010601     C*
162700010601     C                   ENDSR
162800010601     C***
162900010601
163000010601
163100010601
163200010330
163300040119      /TITLE Invio dei dati al punto operativo.
163400040119     C     invio         BEGSR
163500040119     C*
163600040119     C* 1° invio FIVAT
163700040119     C                   reset                   dscmz
163800040119     C                   move      vlrpoi        cmzdst
163900040119     C                   eval      cmzfld = 'FIVATWWR'
164000040119     C                   eval      cmzmbd = vlrhdl
164100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164200040119     C***                if        prmfir = *blanks
164300040119     C                   eval      cmzfla = 'FIVAT00F'
164400040119     C                   eval      cmzmba = 'FIVAT00F'
164500040119     C***                else
164600040119     C***                eval      cmzfla = prmfir
164700040119     C***                eval      cmzmba = prmfir
164800040119     C***                endif
164900040119     C                   eval      cmznrr = *zeros
165000040119     C                   move      §ctrokvt      cmznrr
165100040119     C                   eval      cmzlba = vlrfl1
165200040119     C                   call(e)   'TIS711C'
165300040119     C                   parm                    dscmz
165400040119     C                   parm      *blanks       esito
165500040119     C                   if        %error
165600040119     C                             or cmzerr = '1'
165700040119     C                             or esito  = '1'
165800040119     C                   eval      wrkesito = '3'
165900040119     C                   else
166000040119     C*
166100040119     C* 2° invio FIVAB
166200040119     C                   reset                   dscmz
166300040119     C                   move      vlrpoi        cmzdst
166400040119     C                   eval      cmzfld = vlrfou
166500040119     C                   eval      cmzmbd = vlrhdl
166600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166700040119     C***                if        prmfir = *blanks
166800040119     C                   eval      cmzfla = 'FIVAB00F'
166900040119     C                   eval      cmzmba = 'FIVAB00F'
167000040119     C***                else
167100040119     C***                eval      cmzfla = prmfir
167200040119     C***                eval      cmzmba = prmfir
167300040119     C***                endif
167400040119     C                   eval      cmznrr = *zeros
167500040119     C                   move      §ctrokvb      cmznrr
167600040119     C                   eval      cmzlba = vlrfl1
167700040119     C                   call(e)   'TIS711C'
167800040119     C                   parm                    dscmz
167900040119     C                   parm      *blanks       esito
168000040119     C                   if        %error
168100040119     C                             or cmzerr = '1'
168200040119     C                             or esito  = '1'
168300040119     C                   eval      wrkesito = '3'
168400040119     C                   endif
168500040119     C                   endif
168600040119     C*
168700040119     C                   ENDSR
168800040119     C***
168900010601
169000010601
169100010601
169200010601
169300010601      /TITLE Invio dei dati al punto operativo.
169400010601     C     opeini        BEGSR
169500010601     C*
169600010601     C* Inizializzo flag e contatori operativi
169700010601     C                   movel     '0'           flgGiro           1
169800010601     C                   movel     '0'           flgMulti          1
169900010601     C                   movel     '1'           flgStato          1
170000010615     C                   movel     '0'           flgOk             1
170100010601     C                   z-add     *zeros        cntNonEl         10 0
170200010601     C                   z-add     *zeros        depfil            3 0
170300010601     C                   z-add     *zeros        invfil            3 0
170400010601     C*
170500010601     C                   ENDSR
170600010601     C***
170700070326
170800070326
170900070326
171000070326
171100080916     C     *pssr         BEGSR
171200070329     C*
171300080916     C                   if        %open(tivin00r)
171400080916     C                   close     tivin00r
171500080916     C                   endif
171600080916     C                   if        %open(fivabwwr)
171700080916     C                   close     fivabwwr
171800080916     C                   endif
171900080916     C                   if        %open(fivatwwr)
172000080916     C                   close     fivatwwr
172100080916     C                   endif
172200070326     C*
172300070326     C* Effettuo la chiamata al CLLE preposto
172400080916     C                   call(e)   'TITVVTC'
172500080916     C                   parm                    parccm
172600080916     C                   parm                    parmbr
172700080916     C                   parm      '2'           paropz
172800070326     C*
172900080916     C                   eval      wrkesito = '2'
173000070404     C*
173100080916     C                   seton                                        LR
173200070326     C*
173300080916     C                   ENDSR     '*CANCL'
173400070326     C***
173500070326
173600070326
173700010330
173800010330
173900000613     C     *inzsr        BEGSR
174000990910     C*
174100990910     C     *entry        plist
174200990920     C                   parm                    tivlrds
174300990921     C                   parm      wrkesito      esito
174400000724     C                   parm                    prmlit
174500000710     C                   parm                    prmfir
174600010330     C*
174700010330     C* CALCOLA LA DATA CORRENTE
174800091204     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
174900091204     C                   eval      datcor = %dec(%date() : *ISO)
175000000613     C*
175100000613     C                   ENDSR
175200000613     C***
175300990908
175400091204     Otitv1t2p  E            riepilogo         2
175500990915     O                                              'Upload via Internet'
175600990915     O                                           +1 'Traduzione TIVIN00R -'
175700021025     O                                           +1 'FIVABWWR'
175800990915     O                                           +1 'Testate bolle'
175900990915     O          E            riepilogo   2
176000990915     O                       wrkdata
176100990915     O                       wrkora              +1
176200990915     O                       procname            +1
176300990915     O          E            riepilogo   2
176400990915     O                                              'Cliente..................:'
176500010605     O                       VABCCM        z     +1
176600990915     O          E            riepilogo   2
176700990920     O                                              'Riferimento Strategi.....:'
176800990920     O                       vlrhdl              +1
176900990915     O          E            riepilogo   2
177000990915     O                                              'Giusti...................:'
177100040119     O                       §CTROKVB      2   +  1
177200990915     O          E            riepilogo   2
177300990915     O                                              'Sbagliati e corretti.....:'
177400971022     O                       §CTRMO        2   +  1
177500990915     O          E            riepilogo   2
177600990915     O                                              'Sbagliati e scartati.....:'
177700971022     O                       §CTRNO        2   +  1
177800000613
177900091204     Otitv1t2ps E            testdett          2
178000000613     O                                              'Upload via Internet'
178100000613     O                                           +1 'Traduzione TIVIN00R -'
178200021025     O                                           +1 'FIVABWWR'
178300000613     O                                           +1 'Testate bolle'
178400000616     O          E            testdett    3
178500000613     O                                           +2 'N° rec'
178600000613     O                                           +3 'Anteprima contenuto'
178700000616     O          E            rigadett    2
178800000613     O                       rrnum               +2
178900000621     O                       recko               +3
179000000616     O          E            findett     2
179100000613     O                       wrkdata
179200000613     O                       wrkora              +1
179300000613     O                       procname            +1
179400000616     O          E            findett     2
179500000613     O                                              'Cliente..................:'
179600010605     O                       VABCCM        z     +1
179700000616     O          E            findett     2
179800000613     O                                              'Riferimento Strategi.....:'
179900000613     O                       vlrhdl              +1
180000000616     O          E            findett     2
180100000613     O                                              'Giusti...................:'
180200040119     O                       §CTROKVB      2   +  1
180300000616     O          E            findett     2
180400000613     O                                              'Sbagliati e corretti.....:'
180500000613     O                       §CTRMO        2   +  1
180600000616     O          E            findett     2
180700000613     O                                              'Sbagliati e scartati.....:'
180800000613     O                       §CTRNO        2   +  1
180900000613** CMD - COMANDI CL
181000091204OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1T2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
181100091204OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1T2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
181200091204DLTOVR FILE(TITV1T2P TITV1T2PS) LVL(*)
181300010606
181400000613
181500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181600091204VABATB§VABCCM§VABLNP§VABAAS§VABMGS§VABNRS§VABNSP§VABCBO§VABLNA§VABRSD§VABRD2§
181700091204VABIND§VABCAD§VABLOD§VABPRD§VABNZD§VABGC1§VABGC2§VABCTR§VABTSP§VABIAS§VABVAS§
181800091204VABNAS§VABNCL§VABPKB§VABVLB§VABQFT§VABCAS§VABTIC§VABVCA§VABGCA§VABRMN§VABRMA§
181900091204VABNCD§VABNCA§VABXCO§VABNOT§VABNT2§VABZNC§VABCTM§VABFFD§VABDCR§VABTCR§VABHCR§
182000091204VABCTS§VABFTM§VABVMD§VABVAD§VABGMA§VABGGA§VABGVA§VABTC1§VABTC2§VABSCL§VABANT§
182100091204VABRMO§VABCMO§VABNMO
182200091204
