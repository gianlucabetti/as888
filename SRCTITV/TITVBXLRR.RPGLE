000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200090323     H dftactgrp(*yes)
000300000313     F*
000400090323     Ftntbe01l  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021025     FFIVABwwr  O    E             DISK    usropn
000800030715     Ftitvxlp   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000030715     Ftitvxlps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000090323     D dosr          e ds                  inz
002100090323     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
002200090323     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
002300090323     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
002400090323     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
002500090323     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
002600090323     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
002700090323     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
002800090323     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
002900090323     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
003000090323     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
003100090323     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
003200010330     D dscmz         e ds                  inz
003300010330     D psds           sds
003400010330     D  procname         *PROC
003500090323     D fivabds       e ds                  extname(fivab00f)
003600010330     D tivlrds       e ds                  extname(tivlr00f)
003700090325     D tivlrdsPar    e ds                  extname(tivlr00f) prefix(P_)
003800080222     D tisi95ds      e ds
003900010330     D esito           s              1
004000010330     D prmlit          s             10
004100010330     D prmfir          s             10
004200010330     D wrkesito        s                   like(esito)
004300010330     D wrkdata         s               d
004400010330     D wrkora          s               t
004500010330     D rrnum           s              6  0 INZ(*zeros)
004600010330     D recko           s            150    INZ(*blanks)
004700011113     D depcmd          s            150    INZ(*blanks)
004800090323     D wCTM            s                   LIKE(vabCTM)
004900090323     D wNSR            s              2    INZ
005000090323     D wCRC            s             10    INZ
005100090323     D wKSC            s              7    INZ
005200090323     D wCTR            s              3    INZ
005300090323     D wfivabds        s                   LIKE(fivabds)
005400030715     D*------------------
005500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600030715     D*------------------
005700030715     D SkSplitFLD      S             10    DIM(100)
005800030715     D SkSplitCSV      S            256    DIM(100)
005900030715     D CharCSV         S              1
006000030715     D CharTXT         S              1
006100030715     D CharNUM         S              1
006200090403     D CharSOS         S              1
006300030715     D posDa           S              3  0 INZ(*zeros)
006400030715     D posA            S              3  0 INZ(*zeros)
006500030715     D i               s              3  0 INZ(1)
006600030715     D wGiro           s              1  0 INZ(*zeros)
006700030822     D*------------------
006800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006900030822     D*------------------
007000030822     D posDaDft        S              3  0 INZ(*zeros)
007100030822     D posADft         S              3  0 INZ(*zeros)
007200030822     D j               s              3  0 INZ(1)
007300030822     D wGiroDft        s              1  0 INZ(*zeros)
007400010330     D*------------------
007500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007600010330     D*------------------
007700010330     D WLBDA8          DS                  INZ
007800010330     D  G08DAT                 1      8  0
007900010330     D  G08INV                 9     16  0
008000010330     D  G08ERR                17     17
008100010330     D  G08TGI                18     22  0
008200090323     D*------------------
008300090323     D* SCHIERA X REPERIMENTO SERIE ORM DA TABELLA "OSR"
008400090323     D*------------------
008500090323     D SkCliOSR        s             10    DIM(900)
008600090323     D*
008700090323     D DsCliOSR        ds
008800090323     D  wLnpOSR                       3
008900090323     D  wCliOSR                       7
009000090323     D*
009100090323     D SkLnpNrsOSR     s              5    DIM(900)
009200090323
009300090323
009400031201     D*------------------
009500031201     D* Costanti
009600031201     D*------------------
009700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009900010330
010000010330
010100010330
010200090323     C*
010300090323     C* CARICO LE TABELLE OCCORRENTI
010400090323     C                   exsr      CARTAB
010500090323     C*
010600990915     C                   time                    wrkdata
010700990915     C                   time                    wrkora
010800000913     C                   reset                   rrnum
010900990921     C                   reset                   esito
011000990921     C                   reset                   wrkesito
011100010601     C*
011200010601     C                   exsr      opeini
011300010605     C                   exsr      rwvab
011400010601     C*
011500010601     C                   seton                                        lr
011600010601
011700010601
011800010601
011900010601
012000010601     C*--------------------------------------------------------
012100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012200010601     C*--------------------------------------------------------
012300010601     C     PREELA        BEGSR
012400010601     C*
012500010601     C* SE OCCORRE SPEDIRE IN FILIALE
012600010601     C                   if        invfil <> *zeros and
012700010601     C                             flgGiro = '0'
012800010601     C*
012900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013000010601     C                   eval      flgGiro = '1'
013100000724     C*
013200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013400010601     C     invfil        chain     azorg01l
013500000724     C                   if        %found
013600000724     C                   movel(p)  CMD(1)        depcmd
013700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013800000724     C*
013900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014000011113     C                   Z-ADD     150           LENGH            15 5
014100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014200000724     C                   PARM                    depcmd
014300000724     C                   PARM                    LENGH
014400000724     C*
014500000724     C                   endif
014600000616     C*
014700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014800000616     C                   movel(p)  CMD(2)        depcmd
014900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015000000616     C*
015100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015200011113     C                   Z-ADD     150           LENGH            15 5
015300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015400000616     C                   PARM                    depcmd
015500000616     C                   PARM                    LENGH
015600000613     C*
015700030715     C                   if        not %open(titvxlps)
015800030715     C                   open      titvxlps
015900030715     C                   except    testdett
016000000613     C                   endif
016100010601     C*
016200010601     C                   endif
016300010601     C*
016400010601     C                   ENDSR
016500010601     C***
016600010601
016700010601
016800010601
016900010601
017000010601
017100010601
017200010601
017300010601     C*--------------------------------------------------------
017400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017500010601     C*--------------------------------------------------------
017600010601     C     ENDELA        BEGSR
017700990915     C*
017800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017900080222     C*
018000080222     C* Effettuo lancio TISI95 solo x chiusura
018100080222     C                   CLEAR                   TISI95DS
018200080222     C                   EVAL      I95TLA = 'C'
018300080222     C                   CALL      'TISI95R'
018400080222     C                   PARM                    TISI95DS
018500000613     C*
018600030715     C                   if        %open(titvxlps)
018700000616     C                   except    findett
018800030715     C                   close     titvxlps
018900000613     C                   endif
019000000616     C*
019100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019200011113     C                   Z-ADD     150           LENGH            15 5
019300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019400000616     C                   PARM                    CMD(3)
019500000616     C                   PARM                    LENGH
019600090323     C*
019700090323     C* EFFETTUO CHIAMATA DI "CHIUSURA" AL *PGM ESTERNO CHE EFFETUA LA SCRITTTURA DEI VARI FILE ORM
019800090323     C   54              call(e)   'TITV84R'
019900090323     C                   parm      'C'           InTipl
020000090325     C                   parm                    tivlrdsPar
020100090323     C                   parm                    wfivabds
020200000616     C*
020300010601     C                   ENDSR
020400010601     C***
020500010601
020600010601
020700010601
020800010601
020900000613
021000000613     C*--------------------------------------------------------
021100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021200000613     C*--------------------------------------------------------
021300000621     C     STPR          BEGSR
021400000613     C*
021500030715     C                   if        not %open(titvxlp)
021600030715     C   50              open      titvxlp
021700990915     C                   endif
021800990915     C*
021900030715     C   50              except    riepilogo
022000990915     C*
022100030715     C                   if        %open(titvxlp)
022200030715     C   50              close     titvxlp
022300990914     C                   endif
022400000613     C*
022500000613     C                   ENDSR
022600000613     C***
022700000613
022800010330
022900010601
023000010601
023100010601
023200010330     C*--------------------------------------------------------
023300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023400010330     C*--------------------------------------------------------
023500010605     C     RWVAB         BEGSR
023600010330     C*
023700010330     C                   if        not %open(tivin00r)
023800010330     C                   open      tivin00r
023900010330     C                   endif
024000021025     C                   if        not %open(fivabwwr)
024100021025     C                   open      fivabwwr
024200010330     C                   endif
024300010330     C*
024400010604     C                   clear                   §CTROK
024500010604     C                   clear                   §CTRMO
024600010604     C                   clear                   §CTRNO
024700010330     C*
024800030822     C                   do        *HIVAL
024900010330     C*
025000030822     C                   read      tivin00r                               70
025100010618     C*
025200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025300010618     C                   if        vinflg = '1'
025400010618     C                   eval      flgOk = '1'
025500010618     C                   endif
025600010618     C*
025700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025800040510     C                   z-add     *zeros        wLenVINDTA        4 0
025900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026300040510     C                   z-add     1             wFlgVINDTA
026400040510     C                   leave
026500040510     C                   endif
026600040510     C                   enddo
026700040510     C*
026800040218     C                   if        vindta > *blanks AND
026900040510     C                             wFlgVINDTA = 1
027000010330     C                   add       1             rrnum
027100010330     C*
027200010601     C                   if        *in70 = *off and
027300010330     C                             (vinflg = *blanks
027400010330     C                              or vinflg = '0'
027500010330     C                              or vinflg = '2')
027600010330     C*
027700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027800010711     C                   if        vinflg = *blanks or vinflg = '0'
027900010711     C                   clear                   vinmsg
028000010711     C                   endif
028100010601     C*
028200010330     C                   exsr      inzvar
028300090403     C*
028400090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
028500090403     C* separatori campi
028600090403     C                   if        CharTXT <> *blanks
028700090403     C                   z-add     *zeros        wLenVINDTA        4 0
028800090403     C                   z-add     *zeros        wFlgVINDTA        1 0
028900090403     C                   setoff                                       80
029000090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
029100090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
029200090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
029300090403     C                   if        not *in80
029400090403     C                   seton                                        80
029500090403     C                   else
029600090403     C                   setoff                                       80
029700090403     C                   endif
029800090403     C                   endif
029900090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
030000090403     C                             and *in80 = *on
030100090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
030200090403     C                   endif
030300090403     C                   enddo
030400090403     C                   endif
030500090403     C*
030600010605     C                   exsr      impvab
030700010601     C*
030800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030900010605     C                   eval      depfil = VABLNP
031000010601     C                   exsr      repfil
031100060331     C                   if        VABFGS = *zeros
031200010601     C                   if        depfil = invfil
031300021025     C                   if        vlrpoi = 999
031400030822     C                   move(P)   invfil        VABFGS
031500021025     C                   else
031600030822     C                   move(P)   vlrpoi        VABFGS
031700021025     C                   endif
031800050113     C                   endif
031900060331     C                   endif
032000010601     C*
032100010601     C                   exsr      PREELA
032200090325     C*
032300030822     C                   if        wGiro = 2
032400090323     C*
032500090323     C* Se il codice trattamento merce nn viene passato dal cliente lo valorizzo con il valore
032600090323     C* d default passato come parametro dall'anagrafica clienti/tipo file
032700090323     C                   IF        vabCTM = *blanks
032800090323     C                   EVAL      vabCTM = wCTM
032900090323     C                   ENDIF
033000090323     C*
033100090323     C* Se presente particolarità consegna 'RC' e se il codice
033200090323     C* cliente mittente è valorizzato e numero serie ORM nn passata
033300090323     C* nei parametri verifico se il cliente è abilitato in tabella 'OSR'
033400090323     C                   IF        VABGMA = 'RC'
033500090323     C                   IF        VABCCM > *ZEROS
033600090323     C                   IF        wNSR = *BLANKS
033700090323     C                   Z-ADD     1             jOSR
033800090323     C                   move(p)   VABLNP        wLnpOSR
033900090323     C                   move(p)   VABCCM        wCliOSR
034000090323     C     DsCliOSR      LOOKUP    SkCliOSR(jOSR)                         55
034100090323     C                   IF        %found
034200090323     C                   EVAL      wNSR = %subst(SkLnpNrsOSR(jOSR):4:2)
034300090323     C                   ELSE
034400090323     C                   SETON                                        31
034500090323     C                   EVAL      vinmsg = %trimr(vinmsg)
034600090323     C                             + ' ' + 'ORMNRS(tabellaOSR)'
034700090323     C                   ENDIF
034800090323     C                   ENDIF
034900090323     C                   ENDIF
035000090323     C                   ENDIF
035100090325     C*
035200090325     C* Ebbene...
035300090325     C*
035400090325     C  N31              add       1             §CTROK            7 0
035500090325     C   32              add       1             §CTRMO            7 0
035600090325     C   31              add       1             §CTRNO            7 0
035700090323     C*
035800030822     C  N31              write     FIVAB000
035900090323     C*
036000090323     C* Eseguo operazioni particolari x gestire ritiro contestuale alla consegna
036100090323     C                   if        *IN31 = *OFF AND
036200090323     C                             vabGMA = 'RC'
036300090323     C*
036400090323     C* Accendo 1 indicatore x condizioare la chiamata in chiusura del TITV84R
036500090323     C                   seton                                        54
036600090323     C*
036700090323     C* Effettuo chiamata a *pgm esterno che si occupa della scrittura dei vari file ORM
036800090323     C                   eval      wfivabds = fivabds
036900090325     C                   eval      tivlrdsPar = tivlrds
037000090325     C                   EVAL      P_vlrppt = *blanks
037100090325     C                   EVAL      %subst(P_vlrppt:4:2) = wNSR
037200090323     C                   call      'TITV84R'
037300090323     C                   parm      *blanks       InTipl            1
037400090325     C                   parm                    tivlrdsPar
037500090323     C                   parm                    wfivabds
037600090323     C*
037700090323     C                   endif
037800090323     C*
037900030822     C                   endif
038000010604     C*
038100010604     C                   if        *in31 = *off and
038200010604     C                             *in32 = *off
038300010604     C                   eval      vinflg = '1'
038400010604     C                   else
038500010604     C                   eval      recko = vindta
038600070102     C                   if        %open(titvxlps)
038700070102     C                   except    rigadett
038800070102     C                   endif
038900010604     C                   eval      vinflg = '2'
039000010604     C                   endif
039100010601     C*
039200010604     C                   endif
039300010604     C*
039400010330     C                   else
039500010330     C                   eval      vinflg = '1'
039600010330     C                   endif
039700010601     C*
039800010601     C  N70              update    tivin000
039900010330     C*
040000030822     C  N70              enddo
040100010601     C
040200010601     C*
040300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
040400010601     C                   if        cntNonEl = *zeros or
040500010601     C                             flgMulti = '0'
040600010330     C* Se non ci sono record con errori ...
040700010601     C                   if        §ctrno = 0 and
040800010604     C                             §ctrmo = 0 and
040900010601     C                             flgStato <> '2'
041000010330     C* ... restituisco esito OK.
041100010330     C                   eval      wrkesito = '0'
041200010330     C                   else
041300010330     C                   if        §ctrok > 0
041400010330     C                   eval      wrkesito = '1'
041500010330     C                   else
041600010615     C                   if        flgOk = '0'
041700010615     C                   eval      wrkesito = '2'
041800010615     C                   else
041900010615     C                   eval      wrkesito = '6'
042000010615     C                   endif
042100010330     C                   endif
042200010330     C                   endif
042300010601     C                   else
042400010601     C                   eval      wrkesito = '9'
042500010601     C                   endif
042600010330     C*
042700010330     C                   if        %open(tivin00r)
042800010330     C                   close     tivin00r
042900010330     C                   endif
043000021025     C                   if        %open(fivabwwr)
043100021025     C                   close     fivabwwr
043200010330     C                   endif
043300010601     C*
043400010601     C                   if        vlrpoi <> 999
043500010601     C                   eval      invfil = vlrpoi
043600010601     C                   endif
043700010330     C*
043800010330     C                   if        §ctrok > 0
043900010601     C                             and invfil > *zeros
044000010330     C                   exsr      invio
044100010330     C                   endif
044200010601     C*
044300010618     C                   if        flgGiro = '1'
044400010601     C                   exsr      endela
044500010618     C                   endif
044600010330     C*
044700010330     C                   ENDSR
044800010330     C***
044900010601
045000010601
045100010601
045200010330     C*----------------------------------------------------*
045300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045400010330     C*----------------------------------------------------*
045500010330     C     INZVAR        BEGSR
045600010330     C*
045700030822     C* Inizializzo variabili di wrk
045800020204     C                   Z-ADD     *zeros        Num5_0            5 0
045900090403     C                   z-add     *zeros        invfil
046000030822     C*
046100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046200030822     C                   CLEAR                   FIVAB000
046300030822     C                   CLEAR                   SkSplitCSV
046400030822     C                   EVAL      i = 1
046500030822     C                   EVAL      posDa = *zeros
046600030822     C                   EVAL      posA  = *zeros
046700030822     C*
046800030822     C* Reimposto i valori di default
046900030822     C                   EXSR      DEFCAM
047000030822     C*
047100010330     C                   ENDSR
047200010330     C*----------------------------------------------------*
047300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047400010330     C*----------------------------------------------------*
047500010330     C     DEFCAM        BEGSR
047600030822     C*
047700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047800030715     C* e delimitatore testo.
047900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048200090403     C*
048300090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
048400090403     C                   EVAL      CharSOS = CharNUM
048500030822     C*
048600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
048700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
048800030715     C                   SETON                                        50
048900030715     C                   ELSE
049000030715     C                   SETOFF                                       50
049100030715     C                   ENDIF
049200030822     C*
049300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
049400030822     C                   EVAL      posDaDft = 1
049500030822     C                   EVAL      posADft  = 0
049600030822     C                   EVAL      wGiroDft = 0
049700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
049800030822     C                             posDaDft > 0
049900030822     C*
050000030822     C* Gestisco il 1° giro
050100030822     C                   IF        wGiroDft = 0
050200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
050300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
050400030822     C* Incremento il contatore dei "giri"
050500030822     C                   EVAL      wGiroDft = 1
050600030822     C                   ELSE
050700030822     C                   EVAL      posDaDft = posADft
050800030822     C                   ENDIF
050900030822     C* Eseguo lo scan x trovare la fine del campo corrente
051000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
051100030822     C*
051200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
051300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
051400030822     C                   IF        posDaDft > 0 AND
051500030822     C                             posADft  > 0
051600060331     C* FGS
051700060331     C                   IF        %subst(
051800060331     C                             %subst(vlrppt:posDaDft+1:
051900060331     C                             posADft-posDaDft-1):1:3)
052000060331     C                             = 'FGS'
052100060331     C                   EVAL      PiStr=%trim(%subst(
052200060331     C                             %subst(vlrppt:posDaDft+1:
052300060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400060331     C                   EXSR      CHKNUM
052500060331     C                   IF        PiInt=*on
052600060331     C                   Z-ADD     PiVal         VABFGS
052700060331     C                   ENDIF
052800060331     C                   ENDIF
052900030822     C* NCL
053000030822     C                   IF        %subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):1:3)
053300030822     C                             = 'NCL'
053400030822     C                   EVAL      PiStr=%trim(%subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700030822     C                   EXSR      CHKNUM
053800030822     C                   IF        PiInt=*on
053900030822     C                   Z-ADD     PiVal         VABNCL
054000030822     C                   ENDIF
054100030822     C                   ENDIF
054200030822     C* CCM
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'CCM'
054700030822     C                   EVAL      PiStr=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   EXSR      CHKNUM
055100030822     C                   IF        PiInt=*on
055200030822     C                   Z-ADD     PiVal         VABCCM
055300030822     C                   ENDIF
055400030822     C                   ENDIF
055500030822     C* LNP
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'LNP'
056000030822     C                   EVAL      PiStr=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   EXSR      CHKNUM
056400030822     C                   IF        PiInt=*on
056500030822     C                   Z-ADD     PiVal         VABLNP
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* NRS
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'NRS'
057300030822     C                   EVAL      PiStr=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   EXSR      CHKNUM
057700030822     C                   IF        PiInt=*on
057800030822     C                   Z-ADD     PiVal         VABNRS
057900030822     C                   ENDIF
058000030822     C                   ENDIF
058100030822     C* CTR
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'CTR'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiInt=*on
059100030822     C                   Z-ADD     PiVal         VABCTR
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* PKB
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'PKB'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiNum=*on
060400030822     C                   Z-ADD     PiVal         VABPKB
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* VLB
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'VLB'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiNum=*on
061700030822     C                   Z-ADD     PiVal         VABVLB
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* QFT
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'QFT'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiNum=*on
063000030822     C                   Z-ADD     PiVal         VABQFT
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* CBO
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'CBO'
063800030822     C                   EVAL      VABCBO=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200030822     C* TSP
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'TSP'
064700030822     C                   EVAL      VABTSP=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* VAS
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'VAS'
065600030822     C                   EVAL      VABVAS=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* VCA
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'VCA'
066500030822     C                   EVAL      VABVCA=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* GCA
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'GCA'
067400030822     C                   EVAL      VABGCA=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* CTM
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'CTM'
068300030822     C                   EVAL      VABCTM=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* FFD
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'FFD'
069200030822     C                   EVAL      VABFFD=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* VAD
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VAD'
070100030822     C                   EVAL      VABVAD=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* GMA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'GMA'
071000030822     C                   EVAL      VABGMA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* GGA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GGA'
071900030822     C                   EVAL      VABGGA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* GVA
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'GVA'
072800030822     C                   EVAL      VABGVA=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* TC1
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'TC1'
073700030822     C                   EVAL      VABTC1=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* TC2
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'TC2'
074600030822     C                   EVAL      VABTC2=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* ...
075100030822     C                   ENDIF
075200030822     C                   ENDDO
075300020204     C*
075400010330     C                   ENDSR
075500010607     C*----------------------------------------------------*
075600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075700010607     C*----------------------------------------------------*
075800010607     C     IMPVAB        BEGSR
075900010607     C*
076000010607     C                   SETOFF                                       3132
076100030515     C*
076200010607     C                   MOVEL     datcor        VABAAS
076300010607     C                   MOVE      datcor        VABMGS
076400010607     C*
076500030715     C**********
076600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076700030715     C**********
076800030715     C                   IF        wGiro = *zeros
076900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077000031201     C     minu:maiu     XLATE     vindta        vindta
077100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077200040324     C                   DOW       posDa <= %len(%trimr(vindta))
077300030715     C*
077400030715     C* Gestisco il 1° campo
077500030715     C                   IF        i = 1
077600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077700030715     C                   EVAL      posDa = 1
077800030715     C* Eseguo lo scan x trovare la fine del primo campo
077900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078000030715     C* A questo punto "estraggo" il campo corrente
078100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078200030715     C                                                    (posA-posDa))
078300030715     C* X i campi successivi al 1°
078400030715     C                   ELSE
078500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078600030715     C                   EVAL      posDa = posA + 1
078700030715     C* Eseguo lo scan x trovare la fine del campo corrente
078800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078900030715     C* Gestisco l'ultimo campo
079000030715     C                   IF        posA = *zeros
079100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
079200030715     C* A questo punto "estraggo" il campo corrente
079300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079400030715     C                                                    (posA-posDa))
079500030715     C                   LEAVE
079600030715     C                   ELSE
079700030715     C* A questo punto "estraggo" il campo corrente
079800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079900030715     C                                                    (posA-posDa))
080000030715     C                   ENDIF
080100030715     C                   ENDIF
080200030715     C* Incremento il contatore d campo
080300030715     C                   EVAL      i = i +1
080400030715     C                   ENDDO
080500030715     C*
080600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080700030715     C* delimitazione testo
080800030715     C                   EVAL      i = 1
080900030715     C                   DOW       i < %elem(SkSplitFLD)
081000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081200030715     C                   EVAL      i = i + 1
081300030715     C                   ENDDO
081400030715     C                   EVAL      wGiro = 1
081500030715     C                   ELSE
081600030715     C                   EVAL      wGiro = 2
081700031204     C**********
081800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
081900031204     C* d separatore campo
082000031204     C**********
082100040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
082200040304     C                   EVAL      vindta = ' ' + vindta
082300040304     C                   ENDIF
082400040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
082500040304     C***                EVAL      vindta = %subst(vindta:2)
082600040304     C***                ENDDO
082700031204     C*
082800031204     C                   Z-ADD     *zeros        lunghInput        4 0
082900040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
083000040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
083100040304     C                   EVAL      vindta = vindta + ' '
083200040304     C                   ENDIF
083300040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083400040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083500040304     C***                EVAL      lunghInput = %len(%trim(vindta))
083600040304     C***                ENDDO
083700030715     C**********
083800030715     C* Effettuo lo split del campo dati d input
083900030715     C**********
084000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084100040324     C                   DOW       posDa <= %len(%trimr(vindta))
084200030715     C*
084300030715     C* Gestisco il 1° campo
084400030715     C                   IF        i = 1
084500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084600030715     C                   EVAL      posDa = 1
084700030715     C* Eseguo lo scan x trovare la fine del primo campo
084800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C* X i campi successivi al 1°
085300030715     C                   ELSE
085400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085500030715     C                   EVAL      posDa = posA + 1
085600031204     C* Verifico che nn vi sia il campo nn valorizzato
085700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
085800031204     C* Se campo nn valorizzato skippo al prossimo
085900031204     C                   EVAL      SkSplitCSV(i) = *blanks
086000031204     C                   EVAL      posA  = posA + 1
086100031204     C                   ELSE
086200030715     C* Eseguo lo scan x trovare la fine del campo corrente
086300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086400030715     C* Gestisco l'ultimo campo
086500030715     C                   IF        posA = *zeros
086600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
086700030715     C* A questo punto "estraggo" il campo corrente
086800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086900030715     C                                                    (posA-posDa))
087000030715     C                   LEAVE
087100030715     C                   ELSE
087200030715     C* A questo punto "estraggo" il campo corrente
087300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087400030715     C                                                    (posA-posDa))
087500030715     C                   ENDIF
087600031204     C                   ENDIF
087700030715     C                   ENDIF
087800030715     C* Incremento il contatore d campo
087900030715     C                   EVAL      i = i +1
088000030715     C                   ENDDO
088100030715     C*
088200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088300030715     C* delimitazione testo
088400030715     C                   EVAL      i = 1
088500030715     C                   DOW       i < %elem(SkSplitCSV)
088600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
088700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
088800030715     C                   EVAL      i = i + 1
088900030715     C                   ENDDO
089000030715     C*
089100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089200030715     C                   EVAL      i = 1
089300030715     C                   DOW       i < %elem(SkSplitFLD)
089400030715     C*
089500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
089700030715     C                             SkSplitCSV(i) <> *blanks
089800030721     C*** ATB
089900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090000030721     C                   EVAL      VABATB = SkSplitCSV(i)
090100030721     C                   ENDIF
090200030715     C*** RSD
090300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
090700030715     C* ==
090800030715     C                   ENDIF
090900030721     C*** RD2
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030715     C*** IND
091400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091500030715     C                   EVAL      VABIND = SkSplitCSV(i)
091600030715     C                   ENDIF
091700030715     C*** LOD
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030715     C*** PRD
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030721     C*** NZD
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030715     C*** RMA
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030715     C*** NOT
093400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093600030715     C                   ENDIF
093700030715     C*** NT2
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030715     C*** VCA
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** VAS
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** FFD
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** CBO
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
095500030715     C                   EVAL      VABCBO = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030721     C*** GC1
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** GC2
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** TSP
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** NAS
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** TIC
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** GCA
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
097900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** XCO
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** CTM
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
098700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** TCR
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** CTS
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** FTM
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
099900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** VAD
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** GMA
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
100700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** GGA
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** GVA
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** TC1
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
101900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** TC2
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** SCL
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
102700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** RMO
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** NMO
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*
103800070705     C* Reperisco quindi i campi numerici...
103900030721     C*** CAD
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104100070705     C                   EVAL      VABCAD = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** RMN
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
104500030721     C                   EVAL      PiStr=SkSplitCSV(i)
104600030721     C                   EXSR      CHKNUM
104700030721     C                   IF        PiInt=*on
104800030721     C                   Z-ADD     PiVal         VABRMN
104900030721     C                   ELSE
105000030721     C                   SETON                                        32
105100030721     C                   EVAL      VABRMN = 1
105200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105300030721     C                             + ' ' + 'VABRMN'
105400030721     C                   ENDIF
105500030721     C                   ENDIF
105600030721     C*** NSP
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105800030721     C                   EVAL      PiStr=SkSplitCSV(i)
105900030721     C                   EXSR      CHKNUM
106000030721     C                   IF        PiInt=*on AND
106100030721     C                             PiVal<=9999999
106200030721     C                   Z-ADD     PiVal         VABNSP
106300030721     C                   ELSE
106400030721     C                   SETON                                        32
106500030721     C                   EVAL      VABNSP = *zeros
106600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106700030721     C                             + ' ' + 'VABNSP'
106800030721     C                   ENDIF
106900030721     C                   ENDIF
107000030721     C*** NCL
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107200030721     C                   EVAL      PiStr=SkSplitCSV(i)
107300030721     C                   EXSR      CHKNUM
107400030721     C                   IF        PiInt=*on
107500030721     C                   Z-ADD     PiVal         VABNCL
107600030721     C                   ELSE
107700030721     C                   SETON                                        32
107800030721     C                   EVAL      VABNCL = *zeros
107900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108000030721     C                             + ' ' + 'VABNCL'
108100030721     C                   ENDIF
108200030721     C                   ENDIF
108300030721     C*** PKB
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
108500030721     C                   EVAL      PiStr=SkSplitCSV(i)
108600030721     C                   EXSR      CHKNUM
108700030721     C                   IF        PiNum=*on
108800030721     C                   Z-ADD     PiVal         VABPKB
108900030721     C                   ELSE
109000030721     C                   SETON                                        32
109100030721     C                   EVAL      VABPKB = *zeros
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABPKB'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** CAS
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiNum=*on
110100030721     C                   Z-ADD     PiVal         VABCAS
110200030721     C                   ELSE
110300030721     C                   SETON                                        32
110400030721     C                   EVAL      VABCAS = *zeros
110500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110600030721     C                             + ' ' + 'VABCAS'
110700030721     C                   ENDIF
110800030721     C                   ENDIF
110900030721     C*** CCM
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111100030721     C                   EVAL      PiStr=SkSplitCSV(i)
111200030721     C                   EXSR      CHKNUM
111300030721     C                   IF        PiInt=*on      AND
111400030721     C                             PiVal<=9999999 AND
111500030721     C                             PiVal>*zeros
111600030721     C                   Z-ADD     PiVal         VABCCM
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABCCM = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCCM'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** LNP
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on    AND
112900030721     C                             PiVal<=999   AND
113000030721     C                             PiVal>*zeros
113100030721     C                   Z-ADD     PiVal         VABLNP
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      VABLNP = *zeros
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600030721     C                             + ' ' + 'VABLNP'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** NRS
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on AND
114400030721     C                             PiVal<=99
114500030721     C                   Z-ADD     PiVal         VABNRS
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABNRS'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** LNA
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
115400030721     C                   EVAL      PiStr=SkSplitCSV(i)
115500030721     C                   EXSR      CHKNUM
115600030721     C                   IF        PiInt=*on    AND
115700030721     C                             PiVal<=999   AND
115800030721     C                             PiVal>*zeros
115900030721     C                   Z-ADD     PiVal         VABLNA
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      VABLNA = *zeros
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABLNA'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** CTR
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on  AND
117200030721     C                             PiVal<=999
117300030721     C                   Z-ADD     PiVal         VABCTR
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117700030721     C                             + ' ' + 'VABCTR'
117800030721     C                   ENDIF
117900030721     C                   ENDIF
118000030721     C*** IAS
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiNum=*on
118500030721     C                   Z-ADD     PiVal         VABIAS
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900030721     C                             + ' ' + 'VABIAS'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** VLB
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiNum=*on
119700030721     C                   Z-ADD     PiVal         VABVLB
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABVLB'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** QFT
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABQFT
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABQFT'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** NCD
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiInt=*on      AND
122100030721     C                             PiVal<=9999999
122200030721     C                   Z-ADD     PiVal         VABNCD
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABNCD'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** NCA
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on      AND
123400030721     C                             PiVal<=9999999
123500030721     C                   Z-ADD     PiVal         VABNCA
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABNCA'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** ZNC
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on AND
124700030721     C                             PiVal<=99
124800030721     C                   Z-ADD     PiVal         VABZNC
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABZNC'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** DCR
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on       AND
126000030721     C                             PiVal<=99999999
126100030721     C                   Z-ADD     PiVal         VABDCR
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABDCR'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** HCR
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiInt=*on   AND
127300030721     C                             PiVal<=9999
127400030721     C                   Z-ADD     PiVal         VABHCR
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABHCR'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** VMD
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiNum=*on
128600030721     C                   Z-ADD     PiVal         VABVMD
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABVMD'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** ANT
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on        AND
129800030721     C                             PiVal<=999999999
129900030721     C                   Z-ADD     PiVal         VABANT
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABANT'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** CMO
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on
131100030721     C                   Z-ADD     PiVal         Num5_0
131200030721     C                   MOVEL(p)  Num5_0        VABCMO
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      VABCMO = *zeros
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABCMO'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030715     C*
132100030715     C                   ENDIF
132200030715     C                   EVAL      i = i + 1
132300030715     C                   ENDDO
132400070705     C*
132500070705     C* Considerazioni particolari
132600070705     C*** NZD
132700070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
132800070705     C                   EVAL      VABNZD = *blanks
132900070705     C                   ENDIF
133000070705     C*** CAD
133100070705     C                   IF        VABNZD = *blanks
133200070705     C                   EVAL      PiStr=VABCAD
133300070705     C                   EXSR      CHKNUM
133400070705     C                   IF        PiInt=*on
133500070705     C                   Z-ADD     PiVal         Num5_0
133600070705     C                   MOVEL(p)  Num5_0        VABCAD
133700070705     C                   ELSE
133800070705     C                   SETON                                        32
133900070705     C                   EVAL      VABCAD = *zeros
134000070705     C                   EVAL      vinmsg = %trimr(vinmsg)
134100070705     C                             + ' ' + 'VABCAD'
134200070705     C                   ENDIF
134300070705     C                   ENDIF
134400080222     C*
134500080222     C* Se provincia nn valorizzata la reperisco
134600080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
134700080222     C                   IF        VABPRD = *blanks
134800080222     C                   CLEAR                   TISI95DS
134900080222     C                   EVAL      I95TCN = '3'
135000080222     C                   Z-ADD     datcor        I95DAT
135100080222     C                   EVAL      I95NAR = VABNZD
135200080222     C                   EVAL      I95CAP = VABCAD
135300080222     C                   EVAL      I95LOC = VABLOD
135400080222     C                   CALL      'TISI95R'
135500080222     C                   PARM                    TISI95DS
135600080222     C                   EVAL      VABPRD = O95PRV
135700080222     C                   ENDIF
135800020204     C*
135900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
136000020204     C                   EXSR      CHKIMPDIV
136100030715     C                   ENDIF
136200020204     C*
136300010607     C                   ENDSR
136400010607     C*----------------------------------------------------*
136500010601
136600020204
136700020204     C*----------------------------------------------------*
136800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
136900020204     C*----------------------------------------------------*
137000020204     C     CHKIMPDIV     BEGSR
137100060524     C*
137200060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
137300060524     C                   IF        vabCAS > *zeros
137400060524     C                   IF        vabCBO = '1'
137500060524     C                   EVAL      vabCBO = '4'
137600060524     C                   ENDIF
137700060524     C                   IF        vabCBO = '2'
137800060524     C                   EVAL      vabCBO = '6'
137900060524     C                   ENDIF
138000060524     C                   ENDIF
138100020204     C*
138200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
138300020204     C                   Z-ADD     *zeros        wrkDec            9 9
138400020204     C*
138500020204     C* Come prima cosa effettuo considerazioni sulla divisa
138600020204     C                   IF        vabIAS > *zeros
138700020204     C                   IF        vabVAS <> 'EUR'
138800020204     C                   EVAL      vabVAS =  'ITL'
138900020204     C                   ENDIF
139000020204     C                   ENDIF
139100020204     C*
139200020204     C                   IF        vabCAS > *zeros
139300020204     C                   IF        vabVCA <> 'EUR'
139400020204     C                   EVAL      vabVCA =  'ITL'
139500020204     C                   ENDIF
139600020204     C                   ENDIF
139700020204     C*
139800020204     C                   IF        vabVMD > *zeros
139900020204     C                   IF        vabVAD <> 'EUR'
140000020204     C                   EVAL      vabVAD =  'ITL'
140100020204     C                   ENDIF
140200020204     C                   ENDIF
140300020204     C*
140400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
140500020204     C                   Z-ADD     vabIAS        wrkDec
140600020204     C                   IF        wrkDec > *zeros
140700020204     C                   IF        vabVAS = 'ITL'
140800020204     C                   EVAL      vabIAS = *zeros
140900020204     C                   ENDIF
141000020204     C                   ENDIF
141100020204     C*
141200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
141300020204     C                   Z-ADD     vabCAS        wrkDec
141400020204     C                   IF        wrkDec > *zeros
141500020204     C                   IF        vabVCA = 'ITL'
141600020204     C                   EVAL      vabCAS = *zeros
141700020204     C                   ENDIF
141800020204     C                   ENDIF
141900020204     C*
142000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
142100020204     C                   Z-ADD     vabVMD        wrkDec
142200020204     C                   IF        wrkDec > *zeros
142300020204     C                   IF        vabVAD = 'ITL'
142400020204     C                   EVAL      vabVMD = *zeros
142500020204     C                   ENDIF
142600020204     C                   ENDIF
142700020204     C*
142800020204     C                   ENDSR
142900020204     C***
143000020204
143100010330
143200010330
143300010330     C*----------------------------------------------------*
143400010330     C*  CONTROLLO NUMERICITA' CAMPI
143500010330     C*----------------------------------------------------*
143600010330     C     CHKNUM        BEGSR
143700010330     C*
143800010606     C                   IF        PiDecChr = *blanks
143900030715     C                   EVAL      PiDecChr = CharNUM
144000010606     C                   ENDIF
144100010606     C*
144200010606     C                   CALL(e)   'ISNUMERIC'
144300010330     C                   PARM                    PiStr            30
144400010606     C                   PARM                    PiDecChr          1
144500010330     C                   PARM      *ZEROS        PiVal            30 9
144600010330     C                   PARM      '0'           PiInt             1
144700010330     C                   PARM      '0'           PiNum             1
144800010330     C                   IF        %error
144900010606     C                   EVAL      PiNum=*off
145000010330     C                   ENDIF
145100010330     C*
145200010330     C                   ENDSR
145300010330     C***
145400010330
145500010601
145600010601
145700010601
145800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
145900010601     C     repfil        BEGSR
146000010601     C*
146100010601     C                   if        invfil = *zeros and
146200010601     C                             depfil > *zeros and
146300010629     C                             (vinflg = *blanks or
146400010629     C                              vinflg = *zeros)
146500010601     C
146600010601     C                   eval      invfil = depfil
146700010601     C                   endif
146800010601     C*
146900010601     C                   if        depfil <> invfil and
147000010601     C                             invfil > *zeros
147100010601     C                   eval      flgMulti = '1'
147200010601     C                   if        vinflg = *blanks
147300010601     C                   add       1             cntNonEl
147400010601     C                   endif
147500010601     C                   endif
147600010601     C*
147700010601     C                   if        vinflg = '2'
147800010601     C                   eval      flgStato = '2'
147900010601     C                   endif
148000010601     C*
148100010601     C                   ENDSR
148200010601     C***
148300010601
148400010601
148500010601
148600010330
148700010330
148800010330
148900990920      /TITLE Invio dei dati al punto operativo.
149000000613     C     invio         BEGSR
149100990920     C*
149200990920     C                   reset                   dscmz
149300010601     C                   move      invfil        cmzdst
149400990920     C                   eval      cmzfld = vlrfou
149500990920     C                   eval      cmzmbd = vlrhdl
149600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
149700000710     C                   if        prmfir = *blanks
149800021025     C                   eval      cmzfla = 'FIVAB00F'
149900021025     C                   eval      cmzmba = 'FIVAB00F'
150000000710     C                   else
150100000710     C                   eval      cmzfla = prmfir
150200000710     C                   eval      cmzmba = prmfir
150300000710     C                   endif
150400990920     C                   eval      cmznrr = *zeros
150500990920     C                   move      §ctrok        cmznrr
150600021018     C                   eval      cmzlba = vlrfl1
150700990920     C                   call(e)   'TIS711C'
150800990920     C                   parm                    dscmz
150900990921     C                   parm      *blanks       esito
151000990920     C                   if        %error
151100990920     C                             or cmzerr = '1'
151200990921     C                             or esito  = '1'
151300000710     C                   eval      wrkesito = '3'
151400990920     C                   endif
151500990920     C*
151600000613     C                   ENDSR
151700990910
151800010601
151900010601
152000010601
152100010601
152200010601      /TITLE Invio dei dati al punto operativo.
152300010601     C     opeini        BEGSR
152400010601     C*
152500010601     C* Inizializzo flag e contatori operativi
152600010601     C                   movel     '0'           flgGiro           1
152700010601     C                   movel     '0'           flgMulti          1
152800010601     C                   movel     '1'           flgStato          1
152900010615     C                   movel     '0'           flgOk             1
153000010601     C                   z-add     *zeros        cntNonEl         10 0
153100010601     C                   z-add     *zeros        depfil            3 0
153200010601     C                   z-add     *zeros        invfil            3 0
153300010601     C*
153400010601     C                   ENDSR
153500010601     C***
153600050421
153700050421
153800050421
153900050421     C     *pssr         BEGSR
154000050421     C*
154100050421     C                   eval      wrkesito = '2'
154200050421     C*
154300050421     C                   ENDSR     '*CANCL'
154400050421     C***
154500090323
154600090323
154700090323
154800090323     C*--------------------------------------------------------
154900090323     C* CARTAB - CARICAMENTO TABELLE                          *
155000090323     C*--------------------------------------------------------
155100090323     C     CARTAB        BEGSR
155200090323     C*
155300090323     C* Carico tutta la tabella "OSR" delle LNP/NRS ORM
155400090323     C                   z-add     *zeros        jOSR              3 0
155500090323     C     'OSR'         setll     tntbe01l
155600090323     C                   if        %equal(tntbe01l)
155700090323     C     'OSR'         reade     tntbe01l
155800090323     C                   dow       not %eof(tntbe01l)
155900090323     C                   eval      DOSR = tbeUNI
156000090323     c*
156100090323     c* Verifica e forzatura x nuovi campi aggiunti
156200090323     c                   if        aD§OSRcl6 = *blanks
156300090323     c                   eval      D§OSRcl6 = *zeros
156400090323     c                   endif
156500090323     c                   if        aD§OSRcl7 = *blanks
156600090323     c                   eval      D§OSRcl7 = *zeros
156700090323     c                   endif
156800090323     c                   if        aD§OSRcl8 = *blanks
156900090323     c                   eval      D§OSRcl8 = *zeros
157000090323     c                   endif
157100090323     c                   if        aD§OSRcl9 = *blanks
157200090323     c                   eval      D§OSRcl9 = *zeros
157300090323     c                   endif
157400090323     c                   if        aD§OSRcl10 = *blanks
157500090323     c                   eval      D§OSRcl10 = *zeros
157600090323     c                   endif
157700090323     c                   if        aD§OSRcl11 = *blanks
157800090323     c                   eval      D§OSRcl11 = *zeros
157900090323     c                   endif
158000090323     c                   if        aD§OSRcl12 = *blanks
158100090323     c                   eval      D§OSRcl12 = *zeros
158200090323     c                   endif
158300090323     c                   if        aD§OSRcl13 = *blanks
158400090323     c                   eval      D§OSRcl13 = *zeros
158500090323     c                   endif
158600090323     c                   if        aD§OSRcl14 = *blanks
158700090323     c                   eval      D§OSRcl14 = *zeros
158800090323     c                   endif
158900090323     c                   if        aD§OSRcl15 = *blanks
159000090323     c                   eval      D§OSRcl15 = *zeros
159100090323     c                   endif
159200090323     c                   if        aD§OSRcl16 = *blanks
159300090323     c                   eval      D§OSRcl16 = *zeros
159400090323     c                   endif
159500090323     c*
159600090323     C* Verifico tutti i 16 codici clienti che è possibile indicare x ogni serie
159700090323     C                   if        D§OSRCLI <> *zeros
159800090323     C                   add       1             jOSR
159900090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
160000090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
160100090323     C                   movel     D§OSRCLI      wCliOSR
160200090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
160300090323     C                   endif
160400090323     C*
160500090323     C                   if        D§OSRCL2 <> *zeros
160600090323     C                   add       1             jOSR
160700090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
160800090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
160900090323     C                   movel     D§OSRCL2      wCliOSR
161000090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
161100090323     C                   endif
161200090323     C*
161300090323     C                   if        D§OSRCL3 <> *zeros
161400090323     C                   add       1             jOSR
161500090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
161600090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
161700090323     C                   movel     D§OSRCL3      wCliOSR
161800090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
161900090323     C                   endif
162000090323     C*
162100090323     C                   if        D§OSRCL4 <> *zeros
162200090323     C                   add       1             jOSR
162300090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
162400090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
162500090323     C                   movel     D§OSRCL4      wCliOSR
162600090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
162700090323     C                   endif
162800090323     C*
162900090323     C                   if        D§OSRCL5 <> *zeros
163000090323     C                   add       1             jOSR
163100090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
163200090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
163300090323     C                   movel     D§OSRCL5      wCliOSR
163400090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
163500090323     C                   endif
163600090323     C*
163700090323     C                   if        D§OSRCL6 <> *zeros
163800090323     C                   add       1             jOSR
163900090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
164000090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
164100090323     C                   movel     D§OSRCL6      wCliOSR
164200090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
164300090323     C                   endif
164400090323     C*
164500090323     C                   if        D§OSRCL7 <> *zeros
164600090323     C                   add       1             jOSR
164700090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
164800090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
164900090323     C                   movel     D§OSRCL7      wCliOSR
165000090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
165100090323     C                   endif
165200090323     C*
165300090323     C                   if        D§OSRCL8 <> *zeros
165400090323     C                   add       1             jOSR
165500090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
165600090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
165700090323     C                   movel     D§OSRCL8      wCliOSR
165800090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
165900090323     C                   endif
166000090323     C*
166100090323     C                   if        D§OSRCL9 <> *zeros
166200090323     C                   add       1             jOSR
166300090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
166400090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
166500090323     C                   movel     D§OSRCL9      wCliOSR
166600090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
166700090323     C                   endif
166800090323     C*
166900090323     C                   if        D§OSRCL10 <> *zeros
167000090323     C                   add       1             jOSR
167100090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
167200090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
167300090323     C                   movel     D§OSRCL10     wCliOSR
167400090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
167500090323     C                   endif
167600090323     C*
167700090323     C                   if        D§OSRCL11 <> *zeros
167800090323     C                   add       1             jOSR
167900090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
168000090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
168100090323     C                   movel     D§OSRCL11     wCliOSR
168200090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
168300090323     C                   endif
168400090323     C*
168500090323     C                   if        D§OSRCL12 <> *zeros
168600090323     C                   add       1             jOSR
168700090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
168800090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
168900090323     C                   movel     D§OSRCL12     wCliOSR
169000090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
169100090323     C                   endif
169200090323     C*
169300090323     C                   if        D§OSRCL13 <> *zeros
169400090323     C                   add       1             jOSR
169500090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
169600090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
169700090323     C                   movel     D§OSRCL13     wCliOSR
169800090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
169900090323     C                   endif
170000090323     C*
170100090323     C                   if        D§OSRCL14 <> *zeros
170200090323     C                   add       1             jOSR
170300090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
170400090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
170500090323     C                   movel     D§OSRCL14     wCliOSR
170600090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
170700090323     C                   endif
170800090323     C*
170900090323     C                   if        D§OSRCL15 <> *zeros
171000090323     C                   add       1             jOSR
171100090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
171200090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
171300090323     C                   movel     D§OSRCL15     wCliOSR
171400090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
171500090323     C                   endif
171600090323     C*
171700090323     C                   if        D§OSRCL16 <> *zeros
171800090323     C                   add       1             jOSR
171900090323     C                   eval      SkLnpNrsOSR(jOSR) = tbeKE1
172000090323     C                   eval      wLnpOSR = %subst(tbeKE1:1:3)
172100090323     C                   movel     D§OSRCL16     wCliOSR
172200090323     C                   eval      SkCliOSR(jOSR) = DsCliOSR
172300090323     C                   endif
172400090323     C*
172500090323     C     'OSR'         reade     tntbe01l
172600090323     C                   enddo
172700090323     C                   endif
172800090323     C*
172900090323     C                   ENDSR
173000090323     C***
173100010601
173200010601
173300010330
173400010330
173500000613     C     *inzsr        BEGSR
173600990910     C*
173700990910     C     *entry        plist
173800990920     C                   parm                    tivlrds
173900990921     C                   parm      wrkesito      esito
174000000724     C                   parm                    prmlit
174100000710     C                   parm                    prmfir
174200010330     C*
174300010330     C* CALCOLA LA DATA CORRENTE
174400010330     C                   time                    wn14             14 0
174500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
174600090323     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
174700010330     C                   z-add     wn8           g08dat
174800010330     C                   z-add     *zeros        g08inv
174900010330     C                   movel     '0'           g08err
175000010330     C                   call      'XSRDA8'
175100010330     C                   parm                    wlbda8
175200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
175300000613     C*
175400000613     C                   ENDSR
175500000613     C***
175600990908
175700030715     Otitvxlp   E            riepilogo         2
175800990915     O                                              'Upload via Internet'
175900990915     O                                           +1 'Traduzione TIVIN00R -'
176000021025     O                                           +1 'FIVABWWR'
176100990915     O                                           +1 'Testate bolle'
176200990915     O          E            riepilogo   2
176300990915     O                       wrkdata
176400990915     O                       wrkora              +1
176500990915     O                       procname            +1
176600990915     O          E            riepilogo   2
176700990915     O                                              'Cliente..................:'
176800010605     O                       VABCCM        z     +1
176900990915     O          E            riepilogo   2
177000990920     O                                              'Riferimento Strategi.....:'
177100990920     O                       vlrhdl              +1
177200990915     O          E            riepilogo   2
177300990915     O                                              'Giusti...................:'
177400971022     O                       §CTROK        2   +  1
177500990915     O          E            riepilogo   2
177600990915     O                                              'Sbagliati e corretti.....:'
177700971022     O                       §CTRMO        2   +  1
177800990915     O          E            riepilogo   2
177900990915     O                                              'Sbagliati e scartati.....:'
178000971022     O                       §CTRNO        2   +  1
178100000613
178200030715     Otitvxlps  E            testdett          2
178300000613     O                                              'Upload via Internet'
178400000613     O                                           +1 'Traduzione TIVIN00R -'
178500021025     O                                           +1 'FIVABWWR'
178600000613     O                                           +1 'Testate bolle'
178700000616     O          E            testdett    3
178800000613     O                                           +2 'N° rec'
178900000613     O                                           +3 'Anteprima contenuto'
179000000616     O          E            rigadett    2
179100000613     O                       rrnum               +2
179200000621     O                       recko               +3
179300000616     O          E            findett     2
179400000613     O                       wrkdata
179500000613     O                       wrkora              +1
179600000613     O                       procname            +1
179700000616     O          E            findett     2
179800000613     O                                              'Cliente..................:'
179900010605     O                       VABCCM        z     +1
180000000616     O          E            findett     2
180100000613     O                                              'Riferimento Strategi.....:'
180200000613     O                       vlrhdl              +1
180300000616     O          E            findett     2
180400000613     O                                              'Giusti...................:'
180500000613     O                       §CTROK        2   +  1
180600000616     O          E            findett     2
180700000613     O                                              'Sbagliati e corretti.....:'
180800000613     O                       §CTRMO        2   +  1
180900000616     O          E            findett     2
181000000613     O                                              'Sbagliati e scartati.....:'
181100000613     O                       §CTRNO        2   +  1
181200000613** CMD - COMANDI CL
181300030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
181400030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
181500030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
181600010606
181700000613
