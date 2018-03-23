000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400070306     Ftntbe01l  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021025     FFIVABwwr  O    E             DISK    usropn
000800040119     FFIVATwwr  O    E             DISK    usropn
000900070306     Ftitvy3p   O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070411     F                                     infsr(*pssr)
001200070306     Ftitvy3ps  O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400070411     F                                     infsr(*pssr)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000040113     D*------------
002100040113     D* SCHIEDE A PROGRAMMA
002200040113     D*------------
002300040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002400010330     D*----------------------------------------------------
002500010330     D* DICHIARAZIOINE VARIABILI DI WRK
002600010330     D*----------------------------------------------------
002700010330     D dscmz         e ds                  inz
002800010330     D psds           sds
002900010330     D  procname         *PROC
003000010330     D tivlrds       e ds                  extname(tivlr00f)
003100040128     D tisi95ds      e ds
003200070306     D tivlrds_rc    e ds                  extname(tivlr00f) prefix(rc_)
003300070306     D fivabds       e ds                  extname(fivab00f)
003400070306     D dosr          e ds                  inz
003500080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
003600080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
003700080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
003800080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
003900080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
004000080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
004100080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
004200080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
004300080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
004400080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
004500080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
004600010330     D esito           s              1
004700010330     D prmlit          s             10
004800010330     D prmfir          s             10
004900010330     D wrkesito        s                   like(esito)
005000010330     D wrkdata         s               d
005100010330     D wrkora          s               t
005200010330     D rrnum           s              6  0 INZ(*zeros)
005300010330     D recko           s            150    INZ(*blanks)
005400011113     D depcmd          s            150    INZ(*blanks)
005500040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
005600040510     D parccm          s              8    INZ(*blanks)
005700040119     D parmbr          s             10    INZ(*blanks)
005800040119     D paropz          s              1    INZ(*blanks)
005900040119     D chkcall         s              1    INZ(*blanks)
006000070306     D wNSR            s              2    INZ
006100070306     D wfivabds        s                   LIKE(fivabds)
006200030715     D*------------------
006300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006400030715     D*------------------
006500030715     D SkSplitFLD      S             10    DIM(100)
006600030715     D SkSplitCSV      S            256    DIM(100)
006700030715     D CharCSV         S              1
006800030715     D CharTXT         S              1
006900030715     D CharNUM         S              1
007000030715     D posDa           S              3  0 INZ(*zeros)
007100030715     D posA            S              3  0 INZ(*zeros)
007200030715     D i               s              3  0 INZ(1)
007300030715     D wGiro           s              1  0 INZ(*zeros)
007400060331     D wVATNOT_A       s                   LIKE(VATNOT)
007500060331     D wVATNOT_B       s                   LIKE(VATNOT)
007600060331     D wVATNOT_E       s                   LIKE(VATNOT)
007700060331     D wVABNSP         s                   LIKE(VABNSP) INZ
007800030822     D*------------------
007900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008000030822     D*------------------
008100030822     D posDaDft        S              3  0 INZ(*zeros)
008200030822     D posADft         S              3  0 INZ(*zeros)
008300030822     D j               s              3  0 INZ(1)
008400030822     D wGiroDft        s              1  0 INZ(*zeros)
008500010330     D*------------------
008600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
008700010330     D*------------------
008800010330     D WLBDA8          DS                  INZ
008900010330     D  G08DAT                 1      8  0
009000010330     D  G08INV                 9     16  0
009100010330     D  G08ERR                17     17
009200010330     D  G08TGI                18     22  0
009300080502     D*------------------
009400080502     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
009500080502     D*------------------
009600080502     D SkCliOSR        s             10    DIM(900)
009700080502     D*
009800080502     D DsCliOSR        ds
009900080502     D  wLnpOSR                       3
010000080502     D  wCliOSR                       7
010100080502     D*
010200080502     D SkLnpNrsOSR     s              5    DIM(900)
010300031201     D*------------------
010400031201     D* Costanti
010500031201     D*------------------
010600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010800010330
010900010330
011000010330
011100990915     C                   time                    wrkdata
011200990915     C                   time                    wrkora
011300000913     C                   reset                   rrnum
011400990921     C                   reset                   esito
011500990921     C                   reset                   wrkesito
011600010601     C*
011700010601     C                   exsr      opeini
011800010605     C                   exsr      rwvab
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
016100070306     C                   if        not %open(titvy3ps)
016200070306     C                   open      titvy3ps
016300030715     C                   except    testdett
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
017800990915     C*
017900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
018000040119     C*
018100040119     C* Effettuo la chiamata al CLLE preposto
018200040119     C                   call(e)   'TITVVTC'
018300040510     C                   parm                    parccm
018400040119     C                   parm                    parmbr
018500040119     C                   parm      '2'           paropz
018600050201     C*
018700050201     C* Effettuo lancio TISI95 solo x chiusura
018800050201     C                   CLEAR                   TISI95DS
018900050201     C                   EVAL      I95TLA = 'C'
019000050201     C                   CALL      'TISI95R'
019100050201     C                   PARM                    TISI95DS
019200000613     C*
019300070306     C                   if        %open(titvy3ps)
019400000616     C                   except    findett
019500070306     C                   close     titvy3ps
019600000613     C                   endif
019700000616     C*
019800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019900011113     C                   Z-ADD     150           LENGH            15 5
020000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020100000616     C                   PARM                    CMD(3)
020200000616     C                   PARM                    LENGH
020300070306     C*
020400070306     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
020500070306     C   54              call(e)   'TITV84R'
020600070306     C                   parm      'C'           InTipl
020700070306     C                   parm                    tivlrds_rc
020800070306     C                   parm                    wfivabds
020900000616     C*
021000010601     C                   ENDSR
021100010601     C***
021200010601
021300010601
021400010601
021500000613
021600000613     C*--------------------------------------------------------
021700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021800000613     C*--------------------------------------------------------
021900000621     C     STPR          BEGSR
022000000613     C*
022100070306     C                   if        not %open(titvy3p)
022200070306     C   50              open      titvy3p
022300990915     C                   endif
022400990915     C*
022500030715     C   50              except    riepilogo
022600990915     C*
022700070306     C                   if        %open(titvy3p)
022800070306     C   50              close     titvy3p
022900990914     C                   endif
023000000613     C*
023100000613     C                   ENDSR
023200000613     C***
023300000613
023400010601
023500010601
023600010330     C*--------------------------------------------------------
023700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023800010330     C*--------------------------------------------------------
023900010605     C     RWVAB         BEGSR
024000010330     C*
024100010330     C                   if        not %open(tivin00r)
024200010330     C                   open      tivin00r
024300010330     C                   endif
024400021025     C                   if        not %open(fivabwwr)
024500021025     C                   open      fivabwwr
024600010330     C                   endif
024700040119     C*
024800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024900040119     C                   exsr      prevat
025000040119     C*
025100040119     C                   if        chkcall = '0'
025200040119     C*
025300040119     C                   if        not %open(fivatwwr)
025400040119     C                   open      fivatwwr
025500040119     C                   endif
025600010330     C*
025700040119     C                   clear                   §CTROKVB          7 0
025800040119     C                   clear                   §CTROKVT          7 0
025900040119     C                   clear                   §CTRMO            7 0
026000040119     C                   clear                   §CTRNO            7 0
026100010330     C*
026200030822     C                   do        *HIVAL
026300010330     C*
026400030822     C                   read      tivin00r                               70
026500010618     C*
026600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026700010618     C                   if        vinflg = '1'
026800010618     C                   eval      flgOk = '1'
026900010618     C                   endif
027000040510     C*
027100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027200040510     C                   z-add     *zeros        wLenVINDTA        4 0
027300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027700040510     C                   z-add     1             wFlgVINDTA
027800040510     C                   leave
027900040510     C                   endif
028000040510     C                   enddo
028100010618     C*
028200040510     C                   if        vindta > *blanks AND
028300040510     C                             wFlgVINDTA = 1
028400010330     C                   add       1             rrnum
028500010330     C*
028600010601     C                   if        *in70 = *off and
028700010330     C                             (vinflg = *blanks
028800010330     C                              or vinflg = '0'
028900010330     C                              or vinflg = '2')
029000010330     C*
029100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029200010711     C                   if        vinflg = *blanks or vinflg = '0'
029300010711     C                   clear                   vinmsg
029400010711     C                   endif
029500010601     C*
029600010330     C                   exsr      inzvar
029700010605     C                   exsr      impvab
029800010601     C*
029900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030000010605     C                   eval      depfil = VABLNP
030100010601     C                   exsr      repfil
030200010601     C                   if        depfil = invfil
030300021025     C                   if        vlrpoi = 999
030400030822     C                   move(P)   invfil        VABFGS
030500021025     C                   else
030600030822     C                   move(P)   vlrpoi        VABFGS
030700021025     C                   endif
030800010601     C*
030900010601     C                   exsr      PREELA
031000070306     C*
031100070306     C* Occorre considerare tutte le bolle d questo file come ritiri
031200070306     C* contestuali x cui reperisco il numero serie ORM in tabella 'OSR'
031300070306     C                   EVAL      VABGMA = 'RC'
031400070306     C                   Z-ADD     1             jOSR
031500080502     C                   move(p)   VABLNP        wLnpOSR
031600080502     C                   move(p)   VABCCM        wCliOSR
031700080502     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
031800070306     C                   IF        %found
031900070306     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
032000070306     C                   EVAL      %subst(rc_vlrppt:4:2) = wNSR
032100070306     C                   ELSE
032200070306     C                   SETON                                        31
032300070306     C                   EVAL      vinmsg = %trimr(vinmsg)
032400070306     C                             + ' ' + 'ORMNRS(tabellaOSR)'
032500070306     C                   ENDIF
032600070306     C*
032700070306     C* Forso che l'orm contestuale venga generato con colli e peso = quelli della bolla originale
032800070307     C                   EVAL      %subst(rc_vlrppt:29:1) = 'S'
032900090707     C*
033000090707     C                   EVAL      rc_vlrKSC = vlrKSC
033100090707     C*
033200010601     C*
033300010604     C* Ebbene...
033400010604     C*
033500040119     C  N31              add       1             §CTROKVB
033600040119     C   32              add       1             §CTRMO
033700040119     C   31              add       1             §CTRNO
033800030822     C                   if        wGiro = 2
033900040510     C* Scrivo sempre il VAT
034000040510     C  N31              exsr      wrivat
034100040510     C* Scrivo il VAB solo a rottura d spedizione
034200040119     C                   if        depspe =  *zeros OR
034300040119     C                             depspe <> vabNSP
034400040119     C  N31              exsr      wrivab
034500040119     C  N31              eval      depspe = vabNSP
034600070306     C*
034700070306     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
034800070306     C                   if        *IN31 = *OFF AND
034900070306     C                             vabGMA = 'RC'
035000070306     C*
035100070306     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
035200070306     C                   seton                                        54
035300070306     C*
035400070306     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
035500070306     C                   eval      wfivabds = fivabds
035600070306     C                   call      'TITV84R'
035700070306     C                   parm      *blanks       InTipl            1
035800070306     C                   parm                    tivlrds_rc
035900070306     C                   parm                    wfivabds
036000070306     C                   endif
036100070306     C*
036200040119     C                   endif
036300040119     C                   endif
036400010604     C*
036500010604     C                   if        *in31 = *off and
036600010604     C                             *in32 = *off
036700010604     C                   eval      vinflg = '1'
036800010604     C                   else
036900010604     C                   eval      recko = vindta
037000010604     C                   except    rigadett
037100010604     C                   eval      vinflg = '2'
037200010604     C                   endif
037300010604     C                   endif
037400010601     C*
037500010604     C                   endif
037600010604     C*
037700010330     C                   else
037800010330     C                   eval      vinflg = '1'
037900010330     C                   endif
038000010601     C*
038100010601     C  N70              update    tivin000
038200010330     C*
038300030822     C  N70              enddo
038400040119     C*
038500040119     C                   endif                                                  (endif - chkcall)
038600010601     C*
038700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
038800010601     C                   if        cntNonEl = *zeros or
038900010601     C                             flgMulti = '0'
039000010330     C* Se non ci sono record con errori ...
039100010601     C                   if        §ctrno = 0 and
039200010604     C                             §ctrmo = 0 and
039300010601     C                             flgStato <> '2'
039400010330     C* ... restituisco esito OK.
039500010330     C                   eval      wrkesito = '0'
039600010330     C                   else
039700040119     C                   if        §ctrokvb > 0
039800010330     C                   eval      wrkesito = '1'
039900010330     C                   else
040000010615     C                   if        flgOk = '0'
040100010615     C                   eval      wrkesito = '2'
040200010615     C                   else
040300010615     C                   eval      wrkesito = '6'
040400010615     C                   endif
040500010330     C                   endif
040600010330     C                   endif
040700010601     C                   else
040800010601     C                   eval      wrkesito = '9'
040900010601     C                   endif
041000010330     C*
041100010330     C                   if        %open(tivin00r)
041200010330     C                   close     tivin00r
041300010330     C                   endif
041400021025     C                   if        %open(fivabwwr)
041500021025     C                   close     fivabwwr
041600010330     C                   endif
041700040512     C                   if        %open(fivatwwr)
041800040512     C                   close     fivatwwr
041900040512     C                   endif
042000010601     C*
042100010601     C                   if        vlrpoi <> 999
042200010601     C                   eval      invfil = vlrpoi
042300010601     C                   endif
042400010330     C*
042500040119     C                   if        §ctrokvb > 0
042600010601     C                             and invfil > *zeros
042700010330     C                   exsr      invio
042800010330     C                   endif
042900010601     C*
043000010618     C                   if        flgGiro = '1'
043100010601     C                   exsr      endela
043200010618     C                   endif
043300010330     C*
043400010330     C                   ENDSR
043500010330     C***
043600040119
043700040119
043800040119
043900040119     C*----------------------------------------------------*
044000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
044100040119     C*----------------------------------------------------*
044200040510     C     WRIVAB        BEGSR
044300040119     C*
044400060331     C* Gestisco l'eventuale rottura x numero spedizione
044500060331     C                   if        VABNSP <> wVABNSP
044600070301     C                   if        wVATNOT_E <> *blanks
044700070301     C                   eval      VABCTM = '7Q'
044800070301     C                   endif
044900040119     C                   write     fivab000                                     => scarico il VAB
045000060331     C                   eval      wVABNSP = VABNSP
045100060331     C                   endif
045200040119     C*
045300040119     C                   ENDSR
045400040119
045500040119
045600040119
045700040119     C*----------------------------------------------------*
045800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
045900040119     C*----------------------------------------------------*
046000040510     C     WRIVAT        BEGSR
046100060512     C*
046200060512     C* Inizializzo i campi chiave
046300060512     C                   eval      VATFGS = VABFGS
046400060512     C                   eval      VATAAS = VABAAS
046500060512     C                   eval      VATCCM = VABCCM
046600060512     C                   eval      VATNSP = VABNSP
046700060512     C                   eval      VATLNP = VABLNP
046800040714     C*
046900040714     C* Solo se VATNOT valorizzato scarico il buffer
047000040714     C                   if        VATNOT <> *blanks
047100040119     C*
047200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
047300040510     C                   if        VATTRC = *blanks
047400040510     C                   eval      VATTRC = 'E'
047500040510     C                   endif
047600040510     C                   write     FIVAT000
047700040119     C                   add       1             §CTROKVT
047800040714     C*
047900040714     C                   endif
048000060331     C*
048100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
048200060331     C                   if        wVATNOT_A <> *blanks
048300060331     C                   eval      VATNOT = wVATNOT_A
048400060331     C                   eval      VATTRC = 'A'
048500060331     C                   write     FIVAT000
048600060331     C                   add       1             §CTROKVT
048700060331     C                   endif
048800060331     C*
048900060331     C                   if        wVATNOT_B <> *blanks
049000060331     C                   eval      VATNOT = wVATNOT_B
049100060331     C                   eval      VATTRC = 'B'
049200060331     C                   write     FIVAT000
049300060331     C                   add       1             §CTROKVT
049400060331     C                   endif
049500060331     C*
049600060331     C                   if        wVATNOT_E <> *blanks
049700060331     C                   eval      VATNOT = wVATNOT_E
049800060331     C                   eval      VATTRC = 'E'
049900060331     C                   write     FIVAT000
050000060331     C                   add       1             §CTROKVT
050100060331     C                   endif
050200040119     C*
050300040119     C                   ENDSR
050400040119
050500040119
050600040119
050700040119     C*----------------------------------------------------*
050800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050900040119     C*----------------------------------------------------*
051000040119     C     PREVAT        BEGSR
051100040119     C*
051200040119     C* Compongo il nome del membro da dare al FIVATWWR
051300040119     C                   eval      parmbr = vlrhdl
051400040119     C                   movel     'M'           parmbr
051500040510     C                   eval      parccm = vlrksc
051600040119     C                   eval      paropz = '1'
051700040119     C* Effettuo la chiamata al CLLE preposto
051800040119     C                   call(e)   'TITVVTC'
051900040119     C                   parm                    parccm
052000040119     C                   parm                    parmbr
052100040119     C                   parm                    paropz
052200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052300040119     C                   if        %error
052400040119     C                   movel     '1'           chkcall
052500040119     C                   else
052600040119     C                   movel     '0'           chkcall
052700040119     C                   endif
052800040119     C*
052900040119     C                   ENDSR
053000010601
053100010601
053200010601
053300010330     C*----------------------------------------------------*
053400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053500010330     C*----------------------------------------------------*
053600040119     C     INZVAR        BEGSR
053700010330     C*
053800030822     C* Inizializzo variabili di wrk
053900020204     C                   Z-ADD     *zeros        Num5_0            5 0
054000060331     C                   MOVEL     *blanks       wVATNOT_A
054100060331     C                   MOVEL     *blanks       wVATNOT_B
054200060331     C                   MOVEL     *blanks       wVATNOT_E
054300030822     C*
054400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054500030822     C                   CLEAR                   FIVAB000
054600040119     C                   CLEAR                   FIVAT000
054700030822     C                   CLEAR                   SkSplitCSV
054800030822     C                   EVAL      i = 1
054900030822     C                   EVAL      posDa = *zeros
055000030822     C                   EVAL      posA  = *zeros
055100030822     C*
055200030822     C* Reimposto i valori di default
055300030822     C                   EXSR      DEFCAM
055400030822     C*
055500010330     C                   ENDSR
055600010330     C*----------------------------------------------------*
055700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055800010330     C*----------------------------------------------------*
055900010330     C     DEFCAM        BEGSR
056000030822     C*
056100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056200030715     C* e delimitatore testo.
056300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056600030822     C*
056700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
056900030715     C                   SETON                                        50
057000030715     C                   ELSE
057100030715     C                   SETOFF                                       50
057200030715     C                   ENDIF
057300030822     C*
057400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057500030822     C                   EVAL      posDaDft = 1
057600030822     C                   EVAL      posADft  = 0
057700030822     C                   EVAL      wGiroDft = 0
057800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057900030822     C                             posDaDft > 0
058000030822     C*
058100030822     C* Gestisco il 1° giro
058200030822     C                   IF        wGiroDft = 0
058300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058500030822     C* Incremento il contatore dei "giri"
058600030822     C                   EVAL      wGiroDft = 1
058700030822     C                   ELSE
058800030822     C                   EVAL      posDaDft = posADft
058900030822     C                   ENDIF
059000030822     C* Eseguo lo scan x trovare la fine del campo corrente
059100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059200030822     C*
059300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059500030822     C                   IF        posDaDft > 0 AND
059600030822     C                             posADft  > 0
059700030822     C* NCL
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'NCL'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABNCL
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* CCM
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'CCM'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiInt=*on
062000030822     C                   Z-ADD     PiVal         VABCCM
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* LNP
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'LNP'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiInt=*on
063300030822     C                   Z-ADD     PiVal         VABLNP
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* NRS
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'NRS'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiInt=*on
064600030822     C                   Z-ADD     PiVal         VABNRS
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* CTR
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'CTR'
065400030822     C                   EVAL      PiStr=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   EXSR      CHKNUM
065800030822     C                   IF        PiInt=*on
065900030822     C                   Z-ADD     PiVal         VABCTR
066000030822     C                   ENDIF
066100030822     C                   ENDIF
066200030822     C* PKB
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'PKB'
066700030822     C                   EVAL      PiStr=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   EXSR      CHKNUM
067100030822     C                   IF        PiNum=*on
067200030822     C                   Z-ADD     PiVal         VABPKB
067300030822     C                   ENDIF
067400030822     C                   ENDIF
067500030822     C* VLB
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'VLB'
068000030822     C                   EVAL      PiStr=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   EXSR      CHKNUM
068400030822     C                   IF        PiNum=*on
068500030822     C                   Z-ADD     PiVal         VABVLB
068600030822     C                   ENDIF
068700030822     C                   ENDIF
068800030822     C* QFT
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'QFT'
069300030822     C                   EVAL      PiStr=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   EXSR      CHKNUM
069700030822     C                   IF        PiNum=*on
069800030822     C                   Z-ADD     PiVal         VABQFT
069900030822     C                   ENDIF
070000030822     C                   ENDIF
070100030822     C* CBO
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'CBO'
070600030822     C                   EVAL      VABCBO=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* TSP
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'TSP'
071500030822     C                   EVAL      VABTSP=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* VAS
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'VAS'
072400030822     C                   EVAL      VABVAS=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* VCA
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'VCA'
073300030822     C                   EVAL      VABVCA=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* GCA
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'GCA'
074200030822     C                   EVAL      VABGCA=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* CTM
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'CTM'
075100030822     C                   EVAL      VABCTM=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* FFD
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'FFD'
076000030822     C                   EVAL      VABFFD=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* VAD
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'VAD'
076900030822     C                   EVAL      VABVAD=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300030822     C* GMA
077400030822     C                   IF        %subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):1:3)
077700030822     C                             = 'GMA'
077800030822     C                   EVAL      VABGMA=%trim(%subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100030822     C                   ENDIF
078200030822     C* GGA
078300030822     C                   IF        %subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):1:3)
078600030822     C                             = 'GGA'
078700030822     C                   EVAL      VABGGA=%trim(%subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000030822     C                   ENDIF
079100030822     C* GVA
079200030822     C                   IF        %subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):1:3)
079500030822     C                             = 'GVA'
079600030822     C                   EVAL      VABGVA=%trim(%subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079900030822     C                   ENDIF
080000030822     C* TC1
080100030822     C                   IF        %subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):1:3)
080400030822     C                             = 'TC1'
080500030822     C                   EVAL      VABTC1=%trim(%subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080800030822     C                   ENDIF
080900030822     C* TC2
081000030822     C                   IF        %subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):1:3)
081300030822     C                             = 'TC2'
081400030822     C                   EVAL      VABTC2=%trim(%subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081700030822     C                   ENDIF
081800040714     C* VATTRC
081900040714     C                   IF        %subst(
082000040714     C                             %subst(vlrppt:posDaDft+1:
082100040714     C                             posADft-posDaDft-1):1:3)
082200040714     C                             = 'TRC'
082300040714     C                   EVAL      VATTRC=%trim(%subst(
082400040714     C                             %subst(vlrppt:posDaDft+1:
082500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082600040714     C                   ENDIF
082700030822     C* ...
082800030822     C                   ENDIF
082900030822     C                   ENDDO
083000020204     C*
083100010330     C                   ENDSR
083200010607     C*----------------------------------------------------*
083300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083400010607     C*----------------------------------------------------*
083500010607     C     IMPVAB        BEGSR
083600010607     C*
083700010607     C                   SETOFF                                       3132
083800030515     C*
083900010607     C                   MOVEL     datcor        VABAAS
084000010607     C                   MOVE      datcor        VABMGS
084100010607     C*
084200030715     C**********
084300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084400030715     C**********
084500030715     C                   IF        wGiro = *zeros
084600040113     C*
084700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084900040510     C                             %trim(intcol(3))<>*blanks
085000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085100040113     C                                      %trim(intcol(3))
085200040510     C                   ENDIF
085300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085400031201     C     minu:maiu     XLATE     vindta        vindta
085500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085600030715     C                   DOW       posDa <= %len(%trim(vindta))
085700030715     C*
085800030715     C* Gestisco il 1° campo
085900030715     C                   IF        i = 1
086000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086100030715     C                   EVAL      posDa = 1
086200030715     C* Eseguo lo scan x trovare la fine del primo campo
086300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086400030715     C* A questo punto "estraggo" il campo corrente
086500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086600030715     C                                                    (posA-posDa))
086700030715     C* X i campi successivi al 1°
086800030715     C                   ELSE
086900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087000030715     C                   EVAL      posDa = posA + 1
087100030715     C* Eseguo lo scan x trovare la fine del campo corrente
087200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087300030715     C* Gestisco l'ultimo campo
087400030715     C                   IF        posA = *zeros
087500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C                   LEAVE
088000030715     C                   ELSE
088100030715     C* A questo punto "estraggo" il campo corrente
088200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088300030715     C                                                    (posA-posDa))
088400030715     C                   ENDIF
088500030715     C                   ENDIF
088600030715     C* Incremento il contatore d campo
088700030715     C                   EVAL      i = i +1
088800030715     C                   ENDDO
088900030715     C*
089000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089100030715     C* delimitazione testo
089200030715     C                   EVAL      i = 1
089300030715     C                   DOW       i < %elem(SkSplitFLD)
089400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089600030715     C                   EVAL      i = i + 1
089700030715     C                   ENDDO
089800030715     C                   EVAL      wGiro = 1
089900030715     C                   ELSE
090000030715     C                   EVAL      wGiro = 2
090100031204     C**********
090200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090300031204     C* d separatore campo
090400031204     C**********
090500031204     C                   EVAL      vindta = %trim(vindta)
090600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090700031204     C                   EVAL      vindta = %subst(vindta:2)
090800031204     C                   ENDDO
090900031204     C*
091000031204     C                   Z-ADD     *zeros        lunghInput        4 0
091100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091500031204     C                   ENDDO
091600030715     C**********
091700030715     C* Effettuo lo split del campo dati d input
091800030715     C**********
091900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092000030715     C                   DOW       posDa <= %len(%trim(vindta))
092100030715     C*
092200030715     C* Gestisco il 1° campo
092300030715     C                   IF        i = 1
092400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092500030715     C                   EVAL      posDa = 1
092600030715     C* Eseguo lo scan x trovare la fine del primo campo
092700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092800030715     C* A questo punto "estraggo" il campo corrente
092900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093000030715     C                                                    (posA-posDa))
093100030715     C* X i campi successivi al 1°
093200030715     C                   ELSE
093300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093400030715     C                   EVAL      posDa = posA + 1
093500031204     C* Verifico che nn vi sia il campo nn valorizzato
093600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093700031204     C* Se campo nn valorizzato skippo al prossimo
093800031204     C                   EVAL      SkSplitCSV(i) = *blanks
093900031204     C                   EVAL      posA  = posA + 1
094000031204     C                   ELSE
094100030715     C* Eseguo lo scan x trovare la fine del campo corrente
094200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094300030715     C* Gestisco l'ultimo campo
094400030715     C                   IF        posA = *zeros
094500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094600030715     C* A questo punto "estraggo" il campo corrente
094700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094800030715     C                                                    (posA-posDa))
094900030715     C                   LEAVE
095000030715     C                   ELSE
095100030715     C* A questo punto "estraggo" il campo corrente
095200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095300030715     C                                                    (posA-posDa))
095400030715     C                   ENDIF
095500031204     C                   ENDIF
095600030715     C                   ENDIF
095700030715     C* Incremento il contatore d campo
095800030715     C                   EVAL      i = i +1
095900030715     C                   ENDDO
096000030715     C*
096100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096200030715     C* delimitazione testo
096300030715     C                   EVAL      i = 1
096400030715     C                   DOW       i < %elem(SkSplitCSV)
096500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096700030715     C                   EVAL      i = i + 1
096800030715     C                   ENDDO
096900030715     C*
097000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097100030715     C                   EVAL      i = 1
097200030715     C                   DOW       i < %elem(SkSplitFLD)
097300030715     C*
097400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097600030715     C                             SkSplitCSV(i) <> *blanks
097700030721     C*** ATB
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097900030721     C                   EVAL      VABATB = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030715     C*** RSD
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098600030715     C* ==
098700030715     C                   ENDIF
098800030721     C*** RD2
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030715     C*** IND
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099400030715     C                   EVAL      VABIND = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** LOD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** PRD
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030721     C*** NZD
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030715     C*** RMA
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** NOT
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** NT2
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030715     C*** VCA
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** VAS
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** FFD
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030721     C*** GC1
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** GC2
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** TSP
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104300030721     C                   ENDIF
104400040510     C*** CBO
104500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104700040510     C                   ENDIF
104800030721     C*** NAS
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** TIC
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** GCA
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** XCO
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** CTM
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** TCR
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** CTS
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** FTM
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** VAD
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** GMA
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** GGA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GVA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** TC1
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** TC2
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** SCL
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** RMO
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** NMO
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*
111700030721     C* Reperisco quindi i campi numerici...
111800040510     C*** NSP
111900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112000040510     C                   EVAL      PiStr=SkSplitCSV(i)
112100040510     C                   EXSR      CHKNUM
112200040510     C                   IF        PiInt=*on
112300040510     C                   Z-ADD     PiVal         VABNSP
112400040510     C                   ELSE
112500040510     C                   SETON                                        31
112600040510     C                   EVAL      VABNSP = *zeros
112700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112800040510     C                             + ' ' + 'VABNSP'
112900040510     C                   ENDIF
113000040510     C                   ENDIF
113100070306     C*** RMN
113200070306     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113300070306     C                   EVAL      PiStr=SkSplitCSV(i)
113400070306     C                   EXSR      CHKNUM
113500070306     C                   IF        PiInt=*on
113600070306     C                   Z-ADD     PiVal         VABRMN
113700070306     C                   ELSE
113800070306     C                   SETON                                        31
113900070306     C                   EVAL      VABRMN = *zeros
114000070306     C                   EVAL      vinmsg = %trimr(vinmsg)
114100070306     C                             + ' ' + 'VABRMN'
114200070306     C                   ENDIF
114300070306     C                   ENDIF
114400030721     C*** CAD
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiInt=*on
114900030721     C                   Z-ADD     PiVal         Num5_0
115000030721     C                   MOVEL(p)  Num5_0        VABCAD
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABCAD = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABCAD'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** NCL
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiInt=*on
116300030721     C                   Z-ADD     PiVal         VABNCL
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      VABNCL = *zeros
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800040122     C                             + ' ' + 'VABNCL'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** PKB
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiNum=*on
117600030721     C                   Z-ADD     PiVal         VABPKB
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABPKB = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABPKB'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** CAS
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiNum=*on
118900030721     C                   Z-ADD     PiVal         VABCAS
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      VABCAS = *zeros
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABCAS'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** CCM
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on      AND
120200030721     C                             PiVal<=9999999 AND
120300030721     C                             PiVal>*zeros
120400030721     C                   Z-ADD     PiVal         VABCCM
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABCCM = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900040510     C                             + ' ' + 'VABCCM'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** LNP
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on    AND
121700030721     C                             PiVal<=999   AND
121800030721     C                             PiVal>*zeros
121900030721     C                   Z-ADD     PiVal         VABLNP
122000030721     C                   ELSE
122100040715     C                   SETON                                        32
122200030721     C                   EVAL      VABLNP = *zeros
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400040510     C                             + ' ' + 'VABLNP'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** NRS
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on AND
123200030721     C                             PiVal<=99
123300030721     C                   Z-ADD     PiVal         VABNRS
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABNRS'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** LNA
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiInt=*on    AND
124500030721     C                             PiVal<=999   AND
124600030721     C                             PiVal>*zeros
124700030721     C                   Z-ADD     PiVal         VABLNA
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      VABLNA = *zeros
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABLNA'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** CTR
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on  AND
126000030721     C                             PiVal<=999
126100030721     C                   Z-ADD     PiVal         VABCTR
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABCTR'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** IAS
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiNum=*on
127300030721     C                   Z-ADD     PiVal         VABIAS
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABIAS'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** VLB
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiNum=*on
128500030721     C                   Z-ADD     PiVal         VABVLB
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABVLB'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** QFT
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiNum=*on
129700030721     C                   Z-ADD     PiVal         VABQFT
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABQFT'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** NCD
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on      AND
130900030721     C                             PiVal<=9999999
131000030721     C                   Z-ADD     PiVal         VABNCD
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABNCD'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** NCA
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on      AND
132200030721     C                             PiVal<=9999999
132300030721     C                   Z-ADD     PiVal         VABNCA
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABNCA'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** ZNC
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on AND
133500030721     C                             PiVal<=99
133600030721     C                   Z-ADD     PiVal         VABZNC
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABZNC'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** DCR
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on       AND
134800030721     C                             PiVal<=99999999
134900030721     C                   Z-ADD     PiVal         VABDCR
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABDCR'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** HCR
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on   AND
136100030721     C                             PiVal<=9999
136200030721     C                   Z-ADD     PiVal         VABHCR
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABHCR'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** VMD
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiNum=*on
137400030721     C                   Z-ADD     PiVal         VABVMD
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABVMD'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** ANT
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiInt=*on        AND
138600030721     C                             PiVal<=999999999
138700030721     C                   Z-ADD     PiVal         VABANT
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABANT'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400030721     C*** CMO
139500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139600030721     C                   EVAL      PiStr=SkSplitCSV(i)
139700030721     C                   EXSR      CHKNUM
139800030721     C                   IF        PiInt=*on
139900030721     C                   Z-ADD     PiVal         Num5_0
140000030721     C                   MOVEL(p)  Num5_0        VABCMO
140100030721     C                   ELSE
140200030721     C                   SETON                                        32
140300030721     C                   EVAL      VABCMO = *zeros
140400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140500030721     C                             + ' ' + 'VABCMO'
140600030721     C                   ENDIF
140700030721     C                   ENDIF
140800040510     C*
140900040510     C* Reperisco i campi del FIVAT
141000040510     C*** VATNOT
141100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141300040510     C                   ENDIF
141400040510     C*** VATTRC
141500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141700040510     C                   ENDIF
141800060331     C*** VATNOT_A
141900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142100060331     C                   ENDIF
142200060331     C*** VATNOT_B
142300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142500060331     C                   ENDIF
142600060331     C*** VATNOT_E
142700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142900060331     C                   ENDIF
143000040510     C*
143100030715     C                   ENDIF
143200030715     C                   EVAL      i = i + 1
143300030715     C                   ENDDO
143400040510     C*
143500070306     C* Se NSP e RMN nn valorizzato considero errore bloccante in traduzione
143600070306     C                   IF        VABNSP = *zeros AND VABRMN = *zeros
143700040510     C                   SETON                                        31
143800040510     C                   ENDIF
143900070306     C*
144000070306     C* Se NSP nn valorizzato ma RMN valorizzato => considero RMN come NSP
144100070306     C                   IF        VABNSP = *zeros AND VABRMN <> *zeros
144200070306     C                   Z-ADD     VABRMN        VABNSP
144300070306     C                   ENDIF
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
167700010601
167800070306
167900070306
168000080502     C*--------------------------------------------------------
168100080502     C* CARTAB - CARICAMENTO TABELLE                          *
168200080502     C*--------------------------------------------------------
168300080502     C     CARTAB        BEGSR
168400080502     C*
168500080502     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
168600080502     C                   z-add     *zeros        jOSR              3 0
168700080502     C     'OSR'         setll     tntbe01l
168800080502     C                   if        %equal(tntbe01l)
168900080502     C     'OSR'         reade     tntbe01l
169000080502     C                   dow       not %eof(tntbe01l)
169100080502     C                   eval      DOSR = tbeUNI
169200080502     c*
169300080502     c* Verifica e forzatura x nuovi campi aggiunti
169400080502     c                   if        aD§OSRcl6 = *blanks
169500080502     c                   eval      D§OSRcl6 = *zeros
169600080502     c                   endif
169700080502     c                   if        aD§OSRcl7 = *blanks
169800080502     c                   eval      D§OSRcl7 = *zeros
169900080502     c                   endif
170000080502     c                   if        aD§OSRcl8 = *blanks
170100080502     c                   eval      D§OSRcl8 = *zeros
170200080502     c                   endif
170300080502     c                   if        aD§OSRcl9 = *blanks
170400080502     c                   eval      D§OSRcl9 = *zeros
170500080502     c                   endif
170600080502     c                   if        aD§OSRcl10 = *blanks
170700080502     c                   eval      D§OSRcl10 = *zeros
170800080502     c                   endif
170900080502     c                   if        aD§OSRcl11 = *blanks
171000080502     c                   eval      D§OSRcl11 = *zeros
171100080502     c                   endif
171200080502     c                   if        aD§OSRcl12 = *blanks
171300080502     c                   eval      D§OSRcl12 = *zeros
171400080502     c                   endif
171500080502     c                   if        aD§OSRcl13 = *blanks
171600080502     c                   eval      D§OSRcl13 = *zeros
171700080502     c                   endif
171800080502     c                   if        aD§OSRcl14 = *blanks
171900080502     c                   eval      D§OSRcl14 = *zeros
172000080502     c                   endif
172100080502     c                   if        aD§OSRcl15 = *blanks
172200080502     c                   eval      D§OSRcl15 = *zeros
172300080502     c                   endif
172400080502     c                   if        aD§OSRcl16 = *blanks
172500080502     c                   eval      D§OSRcl16 = *zeros
172600080502     c                   endif
172700080502     c*
172800080502     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
172900080502     C                   if        D§OSRCLI <> *zeros
173000080502     C                   add       1             jOSR
173100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
173200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
173300080502     C                   movel     D§OSRCLI      wCliOSR
173400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
173500080502     C                   endif
173600080502     C*
173700080502     C                   if        D§OSRCL2 <> *zeros
173800080502     C                   add       1             jOSR
173900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
174000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
174100080502     C                   movel     D§OSRCL2      wCliOSR
174200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
174300080502     C                   endif
174400080502     C*
174500080502     C                   if        D§OSRCL3 <> *zeros
174600080502     C                   add       1             jOSR
174700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
174800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
174900080502     C                   movel     D§OSRCL3      wCliOSR
175000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
175100080502     C                   endif
175200080502     C*
175300080502     C                   if        D§OSRCL4 <> *zeros
175400080502     C                   add       1             jOSR
175500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
175600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
175700080502     C                   movel     D§OSRCL4      wCliOSR
175800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
175900080502     C                   endif
176000080502     C*
176100080502     C                   if        D§OSRCL5 <> *zeros
176200080502     C                   add       1             jOSR
176300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
176400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
176500080502     C                   movel     D§OSRCL5      wCliOSR
176600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
176700080502     C                   endif
176800080502     C*
176900080502     C                   if        D§OSRCL6 <> *zeros
177000080502     C                   add       1             jOSR
177100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
177200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
177300080502     C                   movel     D§OSRCL6      wCliOSR
177400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
177500080502     C                   endif
177600080502     C*
177700080502     C                   if        D§OSRCL7 <> *zeros
177800080502     C                   add       1             jOSR
177900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
178000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
178100080502     C                   movel     D§OSRCL7      wCliOSR
178200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
178300080502     C                   endif
178400080502     C*
178500080502     C                   if        D§OSRCL8 <> *zeros
178600080502     C                   add       1             jOSR
178700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
178800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
178900080502     C                   movel     D§OSRCL8      wCliOSR
179000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
179100080502     C                   endif
179200080502     C*
179300080502     C                   if        D§OSRCL9 <> *zeros
179400080502     C                   add       1             jOSR
179500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
179600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
179700080502     C                   movel     D§OSRCL9      wCliOSR
179800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
179900080502     C                   endif
180000080502     C*
180100080502     C                   if        D§OSRCL10 <> *zeros
180200080502     C                   add       1             jOSR
180300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
180400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
180500080502     C                   movel     D§OSRCL10     wCliOSR
180600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
180700080502     C                   endif
180800080502     C*
180900080502     C                   if        D§OSRCL11 <> *zeros
181000080502     C                   add       1             jOSR
181100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
181200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
181300080502     C                   movel     D§OSRCL11     wCliOSR
181400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
181500080502     C                   endif
181600080502     C*
181700080502     C                   if        D§OSRCL12 <> *zeros
181800080502     C                   add       1             jOSR
181900080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
182000080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
182100080502     C                   movel     D§OSRCL12     wCliOSR
182200080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
182300080502     C                   endif
182400080502     C*
182500080502     C                   if        D§OSRCL13 <> *zeros
182600080502     C                   add       1             jOSR
182700080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
182800080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
182900080502     C                   movel     D§OSRCL13     wCliOSR
183000080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
183100080502     C                   endif
183200080502     C*
183300080502     C                   if        D§OSRCL14 <> *zeros
183400080502     C                   add       1             jOSR
183500080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
183600080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
183700080502     C                   movel     D§OSRCL14     wCliOSR
183800080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
183900080502     C                   endif
184000080502     C*
184100080502     C                   if        D§OSRCL15 <> *zeros
184200080502     C                   add       1             jOSR
184300080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
184400080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
184500080502     C                   movel     D§OSRCL15     wCliOSR
184600080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
184700080502     C                   endif
184800080502     C*
184900080502     C                   if        D§OSRCL16 <> *zeros
185000080502     C                   add       1             jOSR
185100080502     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
185200080502     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
185300080502     C                   movel     D§OSRCL16     wCliOSR
185400080502     C                   eval      SkCliOSR(jOSR) = DsCliOSR
185500080502     C                   endif
185600080502     C*
185700080502     C     'OSR'         reade     tntbe01l
185800080502     C                   enddo
185900080502     C                   endif
186000080502     C*
186100080502     C                   ENDSR
186200080502     C***
186300070411
186400080502
186500080502
186600070411     C     *pssr         BEGSR
186700070411     C*
186800070411     C                   if        %open(tivin00r)
186900070411     C                   close     tivin00r
187000070411     C                   endif
187100070411     C                   if        %open(fivabwwr)
187200070411     C                   close     fivabwwr
187300070411     C                   endif
187400070411     C                   if        %open(fivatwwr)
187500070411     C                   close     fivatwwr
187600070411     C                   endif
187700070411     C*
187800070411     C* Effettuo la chiamata al CLLE preposto
187900070411     C                   call(e)   'TITVVTC'
188000070411     C                   parm                    parccm
188100070411     C                   parm                    parmbr
188200070411     C                   parm      '2'           paropz
188300070411     C*
188400070411     C                   eval      wrkesito = '2'
188500070411     C*
188600070411     C                   seton                                        LR
188700070411     C*
188800070411     C                   ENDSR     '*CANCL'
188900070411     C***
189000070411
189100010330
189200010330
189300000613     C     *inzsr        BEGSR
189400990910     C*
189500990910     C     *entry        plist
189600990920     C                   parm                    tivlrds
189700990921     C                   parm      wrkesito      esito
189800000724     C                   parm                    prmlit
189900000710     C                   parm                    prmfir
190000070306     C*
190100070306     C* CARICO LE TABELLE OCCORRENTI
190200070306     C                   exsr      CARTAB
190300010330     C*
190400010330     C* CALCOLA LA DATA CORRENTE
190500010330     C                   time                    wn14             14 0
190600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
190700010330     C                   z-add     wn8           g08dat
190800010330     C                   z-add     *zeros        g08inv
190900010330     C                   movel     '0'           g08err
191000010330     C                   call      'XSRDA8'
191100010330     C                   parm                    wlbda8
191200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
191300000613     C*
191400000613     C                   ENDSR
191500000613     C***
191600990908
191700070306     Otitvy3p   E            riepilogo         2
191800990915     O                                              'Upload via Internet'
191900990915     O                                           +1 'Traduzione TIVIN00R -'
192000021025     O                                           +1 'FIVABWWR'
192100990915     O                                           +1 'Testate bolle'
192200990915     O          E            riepilogo   2
192300990915     O                       wrkdata
192400990915     O                       wrkora              +1
192500990915     O                       procname            +1
192600990915     O          E            riepilogo   2
192700990915     O                                              'Cliente..................:'
192800010605     O                       VABCCM        z     +1
192900990915     O          E            riepilogo   2
193000990920     O                                              'Riferimento Strategi.....:'
193100990920     O                       vlrhdl              +1
193200990915     O          E            riepilogo   2
193300990915     O                                              'Giusti...................:'
193400040119     O                       §CTROKVB      2   +  1
193500990915     O          E            riepilogo   2
193600990915     O                                              'Sbagliati e corretti.....:'
193700971022     O                       §CTRMO        2   +  1
193800990915     O          E            riepilogo   2
193900990915     O                                              'Sbagliati e scartati.....:'
194000971022     O                       §CTRNO        2   +  1
194100000613
194200070306     Otitvy3ps  E            testdett          2
194300000613     O                                              'Upload via Internet'
194400000613     O                                           +1 'Traduzione TIVIN00R -'
194500021025     O                                           +1 'FIVABWWR'
194600000613     O                                           +1 'Testate bolle'
194700000616     O          E            testdett    3
194800000613     O                                           +2 'N° rec'
194900000613     O                                           +3 'Anteprima contenuto'
195000000616     O          E            rigadett    2
195100000613     O                       rrnum               +2
195200000621     O                       recko               +3
195300000616     O          E            findett     2
195400000613     O                       wrkdata
195500000613     O                       wrkora              +1
195600000613     O                       procname            +1
195700000616     O          E            findett     2
195800000613     O                                              'Cliente..................:'
195900010605     O                       VABCCM        z     +1
196000000616     O          E            findett     2
196100000613     O                                              'Riferimento Strategi.....:'
196200000613     O                       vlrhdl              +1
196300000616     O          E            findett     2
196400000613     O                                              'Giusti...................:'
196500040119     O                       §CTROKVB      2   +  1
196600000616     O          E            findett     2
196700000613     O                                              'Sbagliati e corretti.....:'
196800000613     O                       §CTRMO        2   +  1
196900000616     O          E            findett     2
197000000613     O                                              'Sbagliati e scartati.....:'
197100000613     O                       §CTRNO        2   +  1
197200000613** CMD - COMANDI CL
197300070306OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
197400070306OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
197500070306DLTOVR FILE(TITVY3P TITVY3PS) LVL(*)
197600010606
197700000613
197800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
197900040510
198000040122
198100040113
