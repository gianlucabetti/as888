000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400041130     Fazorg01l  IF   e           K DISK
000500041130     Ftivin00r  UF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800060626     Ftitvt4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060626     Ftitvt4ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200041130     D intcol          S            100    DIM(4) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040706     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040709     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040119     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500041130     D*------------------
004600041130     D* DS REPERIMENTO NUMERATORE
004700041130     D*------------------
004800041130     D trul33ds      e ds                  inz
004900041130     D*------------------
005000041130     D* DS ARCHITETTURA
005100041130     D*------------------
005200041130     D kpjba         e ds                  inz
005300030715     D*------------------
005400041130     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005500030715     D*------------------
005600030715     D CharCSV         S              1
005700030715     D CharTXT         S              1
005800030715     D CharNUM         S              1
005900041130     D i               s              5  0 INZ(1)
006000041130     D y               s              3  0 INZ(1)
006100030715     D wGiro           s              1  0 INZ(*zeros)
006200060626     D posDa           S              3  0 INZ(*zeros)
006300060626     D posA            S              3  0 INZ(*zeros)
006400030822     D*------------------
006500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006600030822     D*------------------
006700030822     D posDaDft        S              3  0 INZ(*zeros)
006800030822     D posADft         S              3  0 INZ(*zeros)
006900030822     D j               s              3  0 INZ(1)
007000030822     D wGiroDft        s              1  0 INZ(*zeros)
007100041130     D*------------------
007200060626     D* VARIABILI X LO "SPLITTAMENTO"
007300041130     D*------------------
007400060427     D InStringa       S          10240    VARYING                              (stringa input)
007500041130     D InSepara        S             10                                         (separatore)
007600041130     D OutArray        S               *                                        (array output)
007700041130     D OutErrore       S              1                                         (flag errore)
007800041130     D OutMsg          S             80                                         (messaggio errore)
007900060427     D SkSplitFLD      S             20    DIM(1000) VARYING
008000060427     D SkSplitCSV      S           4096    DIM(1000) VARYING
008100060626     D SkSplitWRK      S           4096    DIM(1000) VARYING
008200060626     D*                                    BASED(OutArray)
008300010330     D*------------------
008400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
008500010330     D*------------------
008600010330     D WLBDA8          DS                  INZ
008700010330     D  G08DAT                 1      8  0
008800010330     D  G08INV                 9     16  0
008900010330     D  G08ERR                17     17
009000010330     D  G08TGI                18     22  0
009100060626     D*------------------
009200060626     D* Costanti
009300060626     D*------------------
009400060626     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009500060626     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009600010330
009700010330
009800010330
009900990915     C                   time                    wrkdata
010000990915     C                   time                    wrkora
010100000913     C                   reset                   rrnum
010200990921     C                   reset                   esito
010300990921     C                   reset                   wrkesito
010400010601     C*
010500010601     C                   exsr      opeini
010600010605     C                   exsr      rwvab
010700010601     C*
010800010601     C                   seton                                        lr
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011400010601     C*--------------------------------------------------------
011500010601     C     PREELA        BEGSR
011600010601     C*
011700010601     C* SE OCCORRE SPEDIRE IN FILIALE
011800010601     C                   if        invfil <> *zeros and
011900010601     C                             flgGiro = '0'
012000010601     C*
012100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012200010601     C                   eval      flgGiro = '1'
012300000724     C*
012400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012600010601     C     invfil        chain     azorg01l
012700000724     C                   if        %found
012800000724     C                   movel(p)  CMD(1)        depcmd
012900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013000000724     C*
013100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013400000724     C                   PARM                    depcmd
013500000724     C                   PARM                    LENGH
013600000724     C*
013700000724     C                   endif
013800000616     C*
013900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014000000616     C                   movel(p)  CMD(2)        depcmd
014100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014200000616     C*
014300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014400011113     C                   Z-ADD     150           LENGH            15 5
014500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014600000616     C                   PARM                    depcmd
014700000616     C                   PARM                    LENGH
014800000613     C*
014900060626     C                   if        not %open(titvt4ps)
015000060626     C                   open      titvt4ps
015100030715     C                   except    testdett
015200000613     C                   endif
015300010601     C*
015400010601     C                   endif
015500010601     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000010601
016100010601
016200010601     C*--------------------------------------------------------
016300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016400010601     C*--------------------------------------------------------
016500010601     C     ENDELA        BEGSR
016600990915     C*
016700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016800040119     C*
016900040119     C* Effettuo la chiamata al CLLE preposto
017000040119     C                   call(e)   'TITVVTC'
017100040119     C                   parm                    parccm
017200040119     C                   parm                    parmbr
017300040119     C                   parm      '2'           paropz
017400050201     C*
017500050201     C* Effettuo lancio TISI95 solo x chiusura
017600050201     C                   CLEAR                   TISI95DS
017700050201     C                   EVAL      I95TLA = 'C'
017800050201     C                   CALL      'TISI95R'
017900050201     C                   PARM                    TISI95DS
018000000613     C*
018100060626     C                   if        %open(titvt4ps)
018200000616     C                   except    findett
018300060626     C                   close     titvt4ps
018400000613     C                   endif
018500000616     C*
018600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018700011113     C                   Z-ADD     150           LENGH            15 5
018800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018900000616     C                   PARM                    CMD(3)
019000000616     C                   PARM                    LENGH
019100000616     C*
019200010601     C                   ENDSR
019300010601     C***
019400010601
019500010601
019600010601
019700000613
019800000613     C*--------------------------------------------------------
019900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020000000613     C*--------------------------------------------------------
020100000621     C     STPR          BEGSR
020200000613     C*
020300060626     C                   if        not %open(titvt4p)
020400060626     C   50              open      titvt4p
020500990915     C                   endif
020600990915     C*
020700030715     C   50              except    riepilogo
020800990915     C*
020900060626     C                   if        %open(titvt4p)
021000060626     C   50              close     titvt4p
021100990914     C                   endif
021200000613     C*
021300000613     C                   ENDSR
021400000613     C***
021500000613
021600010601
021700010601
021800010330     C*--------------------------------------------------------
021900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022000010330     C*--------------------------------------------------------
022100010605     C     RWVAB         BEGSR
022200010330     C*
022300010330     C                   if        not %open(tivin00r)
022400010330     C                   open      tivin00r
022500010330     C                   endif
022600021025     C                   if        not %open(fivabwwr)
022700021025     C                   open      fivabwwr
022800010330     C                   endif
022900040119     C*
023000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023100040119     C                   exsr      prevat
023200040119     C*
023300040119     C                   if        chkcall = '0'
023400040119     C*
023500040119     C                   if        not %open(fivatwwr)
023600040119     C                   open      fivatwwr
023700040119     C                   endif
023800010330     C*
023900040119     C                   clear                   §CTROKVB          7 0
024000040119     C                   clear                   §CTROKVT          7 0
024100040119     C                   clear                   §CTRMO            7 0
024200040119     C                   clear                   §CTRNO            7 0
024300060626     C*
024400060626     C* Eseguo subito inizializzazioni
024500060626     C                   exsr      inzvar
024600060626     C*
024700060626     C* Se necessario e quindi richiesto nei parametri d ingresso =>
024800060626     C* eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024900060626     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025000060626     C                   if        *in51 = *on
025100060626     C                   exsr      impvab
025200060626     C                   endif
025300040706     C*
025400040706     C  N70              do        *HIVAL
025500010330     C*
025600040706     C                   read      tivin00r                               70
025700010618     C*
025800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025900010618     C                   if        vinflg = '1'
026000010618     C                   eval      flgOk = '1'
026100010618     C                   endif
026200060626     C*
026300060626     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
026400060626     C* => gestisco sempre
026500060626     C                   z-add     *zeros        wLenVINDTA        4 0
026600060626     C                   dow       wLenVINDTA < %len(%trim(vindta))
026700060626     C                   eval      wLenVINDTA = wLenVINDTA + 1
026800060626     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
026900060626     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
027000060626     C                   endif
027100060626     C                   enddo
027200060626     C*
027300060626     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027400060626     C                   z-add     *zeros        wLenVINDTA        4 0
027500060626     C                   z-add     *zeros        wFlgVINDTA        1 0
027600060626     C                   dow       wLenVINDTA < %len(%trim(vindta))
027700060626     C                   eval      wLenVINDTA = wLenVINDTA + 1
027800060626     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027900060626     C                   z-add     1             wFlgVINDTA
028000060626     C                   leave
028100060626     C                   endif
028200060626     C                   enddo
028300060626     C*
028400060626     C                   if        vindta > *blanks AND
028500060626     C                             wFlgVINDTA = 1
028600010330     C                   add       1             rrnum
028700010330     C*
028800010601     C                   if        *in70 = *off and
028900010330     C                             (vinflg = *blanks
029000010330     C                              or vinflg = '0'
029100010330     C                              or vinflg = '2')
029200010330     C*
029300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029400010711     C                   if        vinflg = *blanks or vinflg = '0'
029500010711     C                   clear                   vinmsg
029600010711     C                   endif
029700010601     C*
029800010330     C                   exsr      inzvar
029900010605     C                   exsr      impvab
030000010601     C*
030100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030200010605     C                   eval      depfil = VABLNP
030300010601     C                   exsr      repfil
030400010601     C                   if        depfil = invfil
030500021025     C                   if        vlrpoi = 999
030600030822     C                   move(P)   invfil        VABFGS
030700040119     C                   move(P)   invfil        VATFGS
030800021025     C                   else
030900030822     C                   move(P)   vlrpoi        VABFGS
031000040119     C                   move(P)   vlrpoi        VATFGS
031100021025     C                   endif
031200010601     C*
031300010601     C                   exsr      PREELA
031400010601     C*
031500010604     C* Ebbene...
031600010604     C*
031700040119     C  N31              add       1             §CTROKVB
031800040119     C   32              add       1             §CTRMO
031900040119     C   31              add       1             §CTRNO
032000030822     C                   if        wGiro = 2
032100040119     C                   if        depspe =  *zeros OR
032200040119     C                             depspe <> vabNSP
032300040119     C  N31              exsr      wrivab
032400040119     C  N31              eval      depspe = vabNSP
032500040119     C                   endif
032600040119     C                   endif
032700010604     C*
032800010604     C                   if        *in31 = *off and
032900010604     C                             *in32 = *off
033000010604     C                   eval      vinflg = '1'
033100010604     C                   else
033200010604     C                   eval      recko = vindta
033300010604     C                   except    rigadett
033400010604     C                   eval      vinflg = '2'
033500010604     C                   endif
033600010604     C                   endif
033700010601     C*
033800010604     C                   endif
033900010604     C*
034000010330     C                   else
034100010330     C                   eval      vinflg = '1'
034200010330     C                   endif
034300010601     C*
034400010601     C  N70              update    tivin000
034500010330     C*
034600030822     C  N70              enddo
034700040119     C*
034800040119     C                   endif                                                  (endif - chkcall)
034900010601     C*
035000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035100010601     C                   if        cntNonEl = *zeros or
035200010601     C                             flgMulti = '0'
035300010330     C* Se non ci sono record con errori ...
035400010601     C                   if        §ctrno = 0 and
035500010604     C                             §ctrmo = 0 and
035600010601     C                             flgStato <> '2'
035700010330     C* ... restituisco esito OK.
035800010330     C                   eval      wrkesito = '0'
035900010330     C                   else
036000040119     C                   if        §ctrokvb > 0
036100010330     C                   eval      wrkesito = '1'
036200010330     C                   else
036300010615     C                   if        flgOk = '0'
036400010615     C                   eval      wrkesito = '2'
036500010615     C                   else
036600010615     C                   eval      wrkesito = '6'
036700010615     C                   endif
036800010330     C                   endif
036900010330     C                   endif
037000010601     C                   else
037100010601     C                   eval      wrkesito = '9'
037200010601     C                   endif
037300010330     C*
037400010330     C                   if        %open(tivin00r)
037500010330     C                   close     tivin00r
037600010330     C                   endif
037700021025     C                   if        %open(fivabwwr)
037800021025     C                   close     fivabwwr
037900010330     C                   endif
038000040512     C                   if        %open(fivatwwr)
038100040512     C                   close     fivatwwr
038200040512     C                   endif
038300010601     C*
038400010601     C                   if        vlrpoi <> 999
038500010601     C                   eval      invfil = vlrpoi
038600010601     C                   endif
038700010330     C*
038800040119     C                   if        §ctrokvb > 0
038900010601     C                             and invfil > *zeros
039000010330     C                   exsr      invio
039100010330     C                   endif
039200010601     C*
039300010618     C                   if        flgGiro = '1'
039400010601     C                   exsr      endela
039500010618     C                   endif
039600010330     C*
039700010330     C                   ENDSR
039800010330     C***
039900040119
040000040119
040100040119
040200040119     C*----------------------------------------------------*
040300050202     C*  SCARICAMENTO BUFFER RECORDS VAB
040400040119     C*----------------------------------------------------*
040500040119     C     WRIVAB        BEGSR
040600040119     C*
040700040119     C                   write     fivab000                                     => scarico il VAB
040800040119     C*
040900040119     C                   ENDSR
041000040119
041100040119
041200040119
041300040119     C*----------------------------------------------------*
041400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041500040119     C*----------------------------------------------------*
041600041130     C     WRIVAT        BEGSR
041700040706     C*
041800040719     C* Attribuisco la stessa chiave della testata bolle
041900050224     C                   EVAL      VATFGS = VABFGS
042000040719     C                   EVAL      VATAAS = VABAAS
042100040719     C                   EVAL      VATLNP = VABLNP
042200050224     C                   EVAL      VATNRS = VABNRS
042300040719     C                   EVAL      VATNSP = VABNSP
042400060626     C*
042500060626     C* Gestisco il referente destinatario privato
042600060626     C                   EVAL      VATTRC = 'A'
042700060626     C                   EVAL      VATNOT = VABRSD
042800060626     C*
042900060626     C* Scarico il buffer così valorizzato
043000060626     C                   WRITE     FIVAT000
043100060626     C*
043200060626     C* Gestisco il telefono destinatario privato
043300060626     C                   EVAL      VATTRC = 'B'
043400060626     C                   EVAL      VATNOT = VABNOT
043500060626     C*
043600060626     C* Scarico il buffer così valorizzato
043700060626     C                   WRITE     FIVAT000
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
045100041130     C                   eval      parccm = vlrksc
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
047600030822     C*
047700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047800030822     C                   CLEAR                   FIVAB000
047900040119     C                   CLEAR                   FIVAT000
048000030822     C                   CLEAR                   SkSplitCSV
048100030822     C                   EVAL      i = 1
048200060626     C                   EVAL      posDa = *zeros
048300060626     C                   EVAL      posA  = *zeros
048400030822     C*
048500030822     C* Reimposto i valori di default
048600030822     C                   EXSR      DEFCAM
048700030822     C*
048800010330     C                   ENDSR
048900010330     C*----------------------------------------------------*
049000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049100010330     C*----------------------------------------------------*
049200010330     C     DEFCAM        BEGSR
049300040119     C*
049400040119     C* Imposto i valori d default a programma dei campi fondamentali
049500060626     C                   EVAL      VABCCM = 0893661
049600060626     C                   EVAL      VATCCM = 0893661
049700060626     C                   EVAL      VABLNP = 089
049800060626     C                   EVAL      VATLNP = 089
049900040706     C                   EVAL      VABCTR = 000
050000050224     C                   EVAL      VABCTM = '7Q'
050100040119     C                   EVAL      VABCBO = '1'
050200041130     C                   EVAL      VABTSP = 'C'
050300070924     C                   EVAL      VABFGS = 110
050400070924     C                   EVAL      VATFGS = 110
050500060626     C*
050600060626     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050700060626     C* e delimitatore testo.
050800060626     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050900060626     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051000060626     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051100060626     C*
051200060626     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051300060626     C                   SETOFF                                       50
051400060626     C                   IF        %subst(vlrppt:1:1) = 'S'
051500060626     C                   SETON                                        50
051600060626     C                   ENDIF
051700060626     C                   IF        %subst(vlrppt:1:1) = 'I'
051800060626     C                   SETON                                        51
051900060626     C                   ENDIF
052000030822     C*
052100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052200030822     C                   EVAL      posDaDft = 1
052300030822     C                   EVAL      posADft  = 0
052400030822     C                   EVAL      wGiroDft = 0
052500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052600030822     C                             posDaDft > 0
052700030822     C*
052800030822     C* Gestisco il 1° giro
052900030822     C                   IF        wGiroDft = 0
053000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053200030822     C* Incremento il contatore dei "giri"
053300030822     C                   EVAL      wGiroDft = 1
053400030822     C                   ELSE
053500030822     C                   EVAL      posDaDft = posADft
053600030822     C                   ENDIF
053700030822     C* Eseguo lo scan x trovare la fine del campo corrente
053800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053900030822     C*
054000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054200030822     C                   IF        posDaDft > 0 AND
054300030822     C                             posADft  > 0
054400070924     C* FGS
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800070924     C                             = 'FGS'
054900030822     C                   EVAL      PiStr=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   EXSR      CHKNUM
055300030822     C                   IF        PiInt=*on
055400070924     C                   Z-ADD     PiVal         VABFGS
055500070924     C                   Z-ADD     PiVal         VATFGS
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800070924     C* NCL
055900070924     C                   IF        %subst(
056000070924     C                             %subst(vlrppt:posDaDft+1:
056100070924     C                             posADft-posDaDft-1):1:3)
056200070924     C                             = 'NCL'
056300070924     C                   EVAL      PiStr=%trim(%subst(
056400070924     C                             %subst(vlrppt:posDaDft+1:
056500070924     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600070924     C                   EXSR      CHKNUM
056700070924     C                   IF        PiInt=*on
056800070924     C                   Z-ADD     PiVal         VABNCL
056900070924     C                   ENDIF
057000070924     C                   ENDIF
057100030822     C* CCM
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'CCM'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiInt=*on
058100030822     C                   Z-ADD     PiVal         VABCCM
058200040119     C                   Z-ADD     PiVal         VATCCM
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* LNP
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'LNP'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABLNP
059600040119     C                   Z-ADD     PiVal         VATLNP
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* NRS
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'NRS'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiInt=*on
060900030822     C                   Z-ADD     PiVal         VABNRS
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* CTR
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'CTR'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiInt=*on
062200030822     C                   Z-ADD     PiVal         VABCTR
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* PKB
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'PKB'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiNum=*on
063500030822     C                   Z-ADD     PiVal         VABPKB
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* VLB
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'VLB'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiNum=*on
064800030822     C                   Z-ADD     PiVal         VABVLB
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* QFT
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'QFT'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiNum=*on
066100030822     C                   Z-ADD     PiVal         VABQFT
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400030822     C* CBO
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'CBO'
066900030822     C                   EVAL      VABCBO=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* TSP
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'TSP'
067800030822     C                   EVAL      VABTSP=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* VAS
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'VAS'
068700030822     C                   EVAL      VABVAS=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* VCA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'VCA'
069600030822     C                   EVAL      VABVCA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* GCA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'GCA'
070500030822     C                   EVAL      VABGCA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* CTM
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'CTM'
071400030822     C                   EVAL      VABCTM=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* FFD
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'FFD'
072300030822     C                   EVAL      VABFFD=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* VAD
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'VAD'
073200030822     C                   EVAL      VABVAD=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* GMA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GMA'
074100030822     C                   EVAL      VABGMA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* GGA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GGA'
075000030822     C                   EVAL      VABGGA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GVA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GVA'
075900030822     C                   EVAL      VABGVA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* TC1
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'TC1'
076800030822     C                   EVAL      VABTC1=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* TC2
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'TC2'
077700030822     C                   EVAL      VABTC2=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* ...
078200030822     C                   ENDIF
078300030822     C                   ENDDO
078400020204     C*
078500010330     C                   ENDSR
078600010607     C*----------------------------------------------------*
078700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078800010607     C*----------------------------------------------------*
078900010607     C     IMPVAB        BEGSR
079000010607     C*
079100010607     C                   SETOFF                                       3132
079200030515     C*
079300010607     C                   MOVEL     datcor        VABAAS
079400040119     C                   MOVEL     datcor        VATAAS
079500010607     C                   MOVE      datcor        VABMGS
079600010607     C*
079700030715     C**********
079800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079900030715     C**********
080000030715     C                   IF        wGiro = *zeros
080100040113     C*
080200041130     C* Se necessario inserisco qui la forzatura della intesatazione colonne personalizzata
080300041130     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080400041130     C                             %trim(intcol(3))+%trim(intcol(4)) <> *blanks
080500041130     C*
080600041130     C* Effettuo la sovrapposizione d intestazioni
080700041130     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080800041130     C                                      %trim(intcol(3))+%trim(intcol(4))
080900041130     C                   ENDIF
081000060626     C*
081100060626     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081200060626     C     minu:maiu     XLATE     vindta        vindta
081300060626     C*
081400060626     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081500060626     C                   DOW       posDa <= %len(%trimr(vindta))
081600060626     C*
081700060626     C* Gestisco il 1° campo
081800060626     C                   IF        i = 1
081900060626     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082000060626     C                   EVAL      posDa = 1
082100060626     C* Eseguo lo scan x trovare la fine del primo campo
082200060626     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082300060626     C* A questo punto "estraggo" il campo corrente
082400060626     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082500060626     C                                                    (posA-posDa))
082600060626     C* X i campi successivi al 1°
082700060626     C                   ELSE
082800060626     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082900060626     C                   EVAL      posDa = posA + 1
083000060626     C* Eseguo lo scan x trovare la fine del campo corrente
083100060626     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083200060626     C* Gestisco l'ultimo campo
083300060626     C                   IF        posA = *zeros
083400060626     C                   EVAL      posA = %len(%trimr(vindta)) + 1
083500060626     C* A questo punto "estraggo" il campo corrente
083600060626     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083700060626     C                                                    (posA-posDa))
083800060626     C                   LEAVE
083900060626     C                   ELSE
084000060626     C* A questo punto "estraggo" il campo corrente
084100060626     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084200060626     C                                                    (posA-posDa))
084300060626     C                   ENDIF
084400060626     C                   ENDIF
084500060626     C* Incremento il contatore d campo
084600060626     C                   EVAL      i = i +1
084700060626     C                   ENDDO
084800030715     C*
084900041130     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085000030715     C* delimitazione testo
085100030715     C                   EVAL      i = 1
085200041130     C                   DOW       i <= %elem(SkSplitFLD)
085300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085500030715     C                   EVAL      i = i + 1
085600030715     C                   ENDDO
085700030715     C                   EVAL      wGiro = 1
085800030715     C                   ELSE
085900030715     C                   EVAL      wGiro = 2
086000060626     C**********
086100060626     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086200060626     C* d separatore campo
086300060626     C**********
086400060626     C                   EVAL      vindta = %trim(vindta)
086500060626     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086600060626     C                   EVAL      vindta = %subst(vindta:2)
086700060626     C                   ENDDO
086800060626     C
086900060626     C                   Z-ADD     *zeros        lunghInput        4 0
087000060626     C                   EVAL      lunghInput = %len(%trimr(vindta))
087100060626     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087200060626     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087300060626     C                   EVAL      lunghInput = %len(%trim(vindta))
087400060626     C                   ENDDO
087500030715     C**********
087600030715     C* Effettuo lo split del campo dati d input
087700030715     C**********
087800060626     C*
087900060626     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088000060626     C                   DOW       posDa <= %len(%trimr(vindta))
088100060626     C*
088200060626     C* Gestisco il 1° campo
088300060626     C                   IF        i = 1
088400060626     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088500060626     C                   EVAL      posDa = 1
088600060626     C* Eseguo lo scan x trovare la fine del primo campo
088700060626     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088800060626     C* A questo punto "estraggo" il campo corrente
088900060626     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089000060626     C                                                    (posA-posDa))
089100060626     C* X i campi successivi al 1°
089200060626     C                   ELSE
089300060626     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089400060626     C                   EVAL      posDa = posA + 1
089500060626     C* Verifico che nn vi sia il campo nn valorizzato
089600060626     C                   IF        %subst(vindta:posDa:1) = CharCSV
089700060626     C* Se campo nn valorizzato skippo al prossimo
089800060626     C                   EVAL      SkSplitCSV(i) = *blanks
089900060626     C                   EVAL      posA  = posA + 1
090000060626     C                   ELSE
090100060626     C* Eseguo lo scan x trovare la fine del campo corrente
090200060626     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300060626     C* Gestisco l'ultimo campo
090400060626     C                   IF        posA = *zeros
090500060626     C                   EVAL      posA = %len(%trimr(vindta)) + 1
090600060626     C* A questo punto "estraggo" il campo corrente
090700060626     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090800060626     C                                                    (posA-posDa))
090900060626     C                   LEAVE
091000060626     C                   ELSE
091100060626     C* A questo punto "estraggo" il campo corrente
091200060626     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091300060626     C                                                    (posA-posDa))
091400060626     C                   ENDIF
091500060626     C                   ENDIF
091600060626     C                   ENDIF
091700060626     C* Incremento il contatore d campo
091800060626     C                   EVAL      i = i +1
091900060626     C                   ENDDO
092000030715     C*
092100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092200030715     C* delimitazione testo
092300030715     C                   EVAL      i = 1
092400041130     C                   DOW       i <= %elem(SkSplitCSV)
092500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092700030715     C                   EVAL      i = i + 1
092800030715     C                   ENDDO
092900030715     C*
093000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093100030715     C                   EVAL      i = 1
093200041130     C                   DOW       i <= %elem(SkSplitFLD)
093300030715     C*
093400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093500041201     C                   IF        SkSplitFLD(i) <> *blanks
093600030721     C*** ATB
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093800030721     C                   EVAL      VABATB = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030715     C*** RSD
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094500030715     C* ==
094600030715     C                   ENDIF
094700030721     C*** RD2
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030715     C*** IND
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095300030715     C                   EVAL      VABIND = SkSplitCSV(i)
095400030715     C                   ENDIF
095500030715     C*** LOD
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** PRD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030721     C*** NZD
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030715     C*** RMA
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** NOT
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** NT2
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** VCA
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** VAS
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** FFD
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030721     C*** GC1
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** GC2
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** TSP
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** NAS
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** TIC
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** GCA
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** XCO
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** CTM
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** TCR
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** CTS
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** FTM
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** VAD
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** GMA
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** GGA
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** GVA
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** TC1
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TC2
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** SCL
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** RMO
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** NMO
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107000030721     C                   ENDIF
107100060626     C*** CBO
107200060626     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107300060626     C                   IF        SkSplitCSV(i) = 'CA'
107400060626     C                   EVAL      VABCBO = '4'
107500060626     C                   ENDIF
107600060626     C                   ENDIF
107700041130     C*
107800050224     C* Reperisco quindi i campi numerici...
107900040706     C*** NSP
108000040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108100040706     C                   EVAL      PiStr=SkSplitCSV(i)
108200040706     C                   EXSR      CHKNUM
108300041130     C                   IF        PiInt=*on AND PiVal <> *zeros
108400040706     C                   Z-ADD     PiVal         VABNSP
108500040706     C                   ENDIF
108600040706     C                   ENDIF
108700040706     C*** RMN
108800040706     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108900040706     C                   EVAL      PiStr=SkSplitCSV(i)
109000040706     C                   EXSR      CHKNUM
109100040706     C                   IF        PiInt=*on
109200040706     C                   Z-ADD     PiVal         VABRMN
109300040706     C                   ELSE
109400040706     C                   SETON                                        32
109500040706     C                   EVAL      VABRMN = 1
109600040706     C                   EVAL      vinmsg = %trimr(vinmsg)
109700040706     C                             + ' ' + 'VABRMN'
109800040706     C                   ENDIF
109900040706     C                   ENDIF
110000030721     C*** CAD
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110200030721     C                   EVAL      PiStr=SkSplitCSV(i)
110300030721     C                   EXSR      CHKNUM
110400030721     C                   IF        PiInt=*on
110500030721     C                   Z-ADD     PiVal         Num5_0
110600030721     C                   MOVEL(p)  Num5_0        VABCAD
110700030721     C                   ELSE
110800030721     C                   SETON                                        32
110900030721     C                   EVAL      VABCAD = *zeros
111000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111100030721     C                             + ' ' + 'VABCAD'
111200030721     C                   ENDIF
111300030721     C                   ENDIF
111400030721     C*** NCL
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiInt=*on
111900030721     C                   Z-ADD     PiVal         VABNCL
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      VABNCL = *zeros
112300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112400040706     C                             + ' ' + 'VABNCL'
112500030721     C                   ENDIF
112600030721     C                   ENDIF
112700030721     C*** PKB
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112900030721     C                   EVAL      PiStr=SkSplitCSV(i)
113000030721     C                   EXSR      CHKNUM
113100030721     C                   IF        PiNum=*on
113200030721     C                   Z-ADD     PiVal         VABPKB
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      VABPKB = *zeros
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700030721     C                             + ' ' + 'VABPKB'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** CAS
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiNum=*on
114500030721     C                   Z-ADD     PiVal         VABCAS
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABCAS = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABCAS'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** LNP
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on    AND
115800030721     C                             PiVal<=999   AND
115900030721     C                             PiVal>*zeros
116000030721     C                   Z-ADD     PiVal         VABLNP
116100040119     C                   Z-ADD     PiVal         VATLNP
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABLNP = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600040119     C                             + ' ' + 'VABLNP VATLNP'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** NRS
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on AND
117400030721     C                             PiVal<=99
117500030721     C                   Z-ADD     PiVal         VABNRS
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900030721     C                             + ' ' + 'VABNRS'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** LNA
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiInt=*on    AND
118700030721     C                             PiVal<=999   AND
118800030721     C                             PiVal>*zeros
118900030721     C                   Z-ADD     PiVal         VABLNA
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      VABLNA = *zeros
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABLNA'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** CTR
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on  AND
120200030721     C                             PiVal<=999
120300030721     C                   Z-ADD     PiVal         VABCTR
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABCTR'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** IAS
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiNum=*on
121500030721     C                   Z-ADD     PiVal         VABIAS
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABIAS'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** VLB
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiNum=*on
122700030721     C                   Z-ADD     PiVal         VABVLB
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABVLB'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** QFT
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiNum=*on
123900030721     C                   Z-ADD     PiVal         VABQFT
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABQFT'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600050202     C*** NCD
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000050202     C                   IF        PiInt=*on
125100060626     C                   Z-ADD     PiVal         VABNCD
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABNCD'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800050202     C*** NCA
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200050202     C                   IF        PiInt=*on
126300060626     C                   Z-ADD     PiVal         VABNCA
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABNCA'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** ZNC
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on AND
127500030721     C                             PiVal<=99
127600030721     C                   Z-ADD     PiVal         VABZNC
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABZNC'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** DCR
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on       AND
128800030721     C                             PiVal<=99999999
128900030721     C                   Z-ADD     PiVal         VABDCR
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABDCR'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** HCR
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on   AND
130100030721     C                             PiVal<=9999
130200030721     C                   Z-ADD     PiVal         VABHCR
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABHCR'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** VMD
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiNum=*on
131400030721     C                   Z-ADD     PiVal         VABVMD
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABVMD'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** ANT
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on        AND
132600030721     C                             PiVal<=999999999
132700030721     C                   Z-ADD     PiVal         VABANT
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABANT'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** CMO
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on
133900030721     C                   Z-ADD     PiVal         Num5_0
134000030721     C                   MOVEL(p)  Num5_0        VABCMO
134100030721     C                   ELSE
134200030721     C                   SETON                                        32
134300030721     C                   EVAL      VABCMO = *zeros
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABCMO'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030715     C*
134900030715     C                   ENDIF
135000030715     C                   EVAL      i = i + 1
135100030715     C                   ENDDO
135200040706     C*
135300040706     C* Se nn valorizzata la provincia destinatario la calcolo tramite TISI95
135400040706     C* Reperisco la provincia dal CAP e dalla località
135500040706     C                   IF        VABCAD <> *blanks
135600040706     C                   CLEAR                   TISI95DS
135700041130     C                   EVAL      I95TCN = '4'
135800040706     C                   Z-ADD     datcor        I95DAT
135900040706     C                   EVAL      I95CAP = VABCAD
136000040706     C                   EVAL      I95LOC = VABLOD
136100040706     C                   CALL      'TISI95R'
136200040706     C                   PARM                    TISI95DS
136300040706     C                   EVAL      VABPRD = O95PRV
136400040706     C                   ENDIF
136500040719     C*
136600050224     C* Se valore NSP passato dal clinte nn è valido o nn passato => stacco un numeratore da AZNUM
136700040719     C                   IF        VABNSP = *zeros
136800050224     C                   clear                   TRUL33DS
136900050224     C                   eval      I33OPE = *zeros
137000050224     C                   eval      I33CNU = 302
137100050224     C                   eval      I33NUM = 1
137200050224     C                   movel     TRUL33DS      KPJBU
137300050224     C                   call      'TRUL33R'
137400050224     C                   parm                    KPJBA
137500050224     C                   movel     KPJBU         TRUL33DS
137600050224     C                   if        O33ERR = *zeros
137700050224     C                   z-add     O33NRF        VABNSP
137800050224     C                   else
137900050224     C                   seton                                        31
138000050224     C                   EVAL      VABNSP = *zeros
138100050224     C                   EVAL      vinmsg = %trimr(vinmsg)
138200050224     C                             + ' ' + 'VABNSP VATNSP'
138300050224     C                   endif
138400050224     C                   ENDIF
138500040719     C*
138600040719     C* Se LNP nn valorizzato considero errore bloccante in traduzione
138700040719     C                   IF        VABLNP = *zeros
138800040719     C                   SETON                                        31
138900041130     C                   EVAL      vinmsg = %trimr(vinmsg)
139000041130     C                             + ' ' + 'VABLNP'
139100040719     C                   ENDIF
139200040706     C*
139300040706     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
139400040706     C* d valorizzarli sempre entrambi
139500040706     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
139600040706     C                   Z-ADD     VABNSP        VABRMN
139700040706     C                   ENDIF
139800040706     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
139900040706     C                   MOVEL     VABRMN        VABRMA
140000040706     C                   ENDIF
140100060626     C*
140200060626     C* Eseguo scrittura estensioni VAT
140300060626     C  N31              EXSR      wrivat
140400060927     C*
140500060927     C* Considerazioni particolari: se nn richiesta espressamente bolla in contrassegno
140600060927     C* nn tengo conto dell'importo passato
140700060927     C                   IF        VABCBO = '4'
140800060927     C                   ELSE
140900060927     C                   EVAL      VABCAS = *zeros
141000060927     C                   ENDIF
141100040113     C*
141200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
141300040113     C                   IF        VABCAS > *zeros
141400040119     C                   EVAL      VABVCA = 'EUR'
141500040119     C                   IF        VABCBO = '1'
141600040113     C                   EVAL      VABCBO = '4'
141700040119     C                   ENDIF
141800040119     C                   IF        VABCBO = '2'
141900040119     C                   EVAL      VABCBO = '6'
142000040119     C                   ENDIF
142100040113     C                   ENDIF
142200020204     C*
142300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
142400020204     C                   EXSR      CHKIMPDIV
142500030715     C                   ENDIF
142600020204     C*
142700010607     C                   ENDSR
142800010607     C*----------------------------------------------------*
142900010601
143000020204
143100020204     C*----------------------------------------------------*
143200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
143300020204     C*----------------------------------------------------*
143400020204     C     CHKIMPDIV     BEGSR
143500020204     C*
143600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
143700020204     C                   Z-ADD     *zeros        wrkDec            9 9
143800020204     C*
143900020204     C* Come prima cosa effettuo considerazioni sulla divisa
144000020204     C                   IF        vabIAS > *zeros
144100020204     C                   IF        vabVAS <> 'EUR'
144200020204     C                   EVAL      vabVAS =  'ITL'
144300020204     C                   ENDIF
144400020204     C                   ENDIF
144500020204     C*
144600020204     C                   IF        vabCAS > *zeros
144700020204     C                   IF        vabVCA <> 'EUR'
144800020204     C                   EVAL      vabVCA =  'ITL'
144900020204     C                   ENDIF
145000020204     C                   ENDIF
145100020204     C*
145200020204     C                   IF        vabVMD > *zeros
145300020204     C                   IF        vabVAD <> 'EUR'
145400020204     C                   EVAL      vabVAD =  'ITL'
145500020204     C                   ENDIF
145600020204     C                   ENDIF
145700020204     C*
145800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
145900020204     C                   Z-ADD     vabIAS        wrkDec
146000020204     C                   IF        wrkDec > *zeros
146100020204     C                   IF        vabVAS = 'ITL'
146200020204     C                   EVAL      vabIAS = *zeros
146300020204     C                   ENDIF
146400020204     C                   ENDIF
146500020204     C*
146600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
146700020204     C                   Z-ADD     vabCAS        wrkDec
146800020204     C                   IF        wrkDec > *zeros
146900020204     C                   IF        vabVCA = 'ITL'
147000020204     C                   EVAL      vabCAS = *zeros
147100020204     C                   ENDIF
147200020204     C                   ENDIF
147300020204     C*
147400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
147500020204     C                   Z-ADD     vabVMD        wrkDec
147600020204     C                   IF        wrkDec > *zeros
147700020204     C                   IF        vabVAD = 'ITL'
147800020204     C                   EVAL      vabVMD = *zeros
147900020204     C                   ENDIF
148000020204     C                   ENDIF
148100020204     C*
148200020204     C                   ENDSR
148300020204     C***
148400020204
148500010330
148600010330
148700010330     C*----------------------------------------------------*
148800010330     C*  CONTROLLO NUMERICITA' CAMPI
148900010330     C*----------------------------------------------------*
149000010330     C     CHKNUM        BEGSR
149100010330     C*
149200010606     C                   IF        PiDecChr = *blanks
149300030715     C                   EVAL      PiDecChr = CharNUM
149400010606     C                   ENDIF
149500010606     C*
149600010606     C                   CALL(e)   'ISNUMERIC'
149700010330     C                   PARM                    PiStr            30
149800010606     C                   PARM                    PiDecChr          1
149900010330     C                   PARM      *ZEROS        PiVal            30 9
150000010330     C                   PARM      '0'           PiInt             1
150100010330     C                   PARM      '0'           PiNum             1
150200010330     C                   IF        %error
150300010606     C                   EVAL      PiNum=*off
150400010330     C                   ENDIF
150500010330     C*
150600010330     C                   ENDSR
150700010330     C***
150800010330
150900010601
151000010601
151100010601
151200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
151300010601     C     repfil        BEGSR
151400010601     C*
151500010601     C                   if        invfil = *zeros and
151600010601     C                             depfil > *zeros and
151700010629     C                             (vinflg = *blanks or
151800010629     C                              vinflg = *zeros)
151900010601     C
152000010601     C                   eval      invfil = depfil
152100010601     C                   endif
152200010601     C*
152300010601     C                   if        depfil <> invfil and
152400010601     C                             invfil > *zeros
152500010601     C                   eval      flgMulti = '1'
152600010601     C                   if        vinflg = *blanks
152700010601     C                   add       1             cntNonEl
152800010601     C                   endif
152900010601     C                   endif
153000010601     C*
153100010601     C                   if        vinflg = '2'
153200010601     C                   eval      flgStato = '2'
153300010601     C                   endif
153400010601     C*
153500010601     C                   ENDSR
153600010601     C***
153700010601
153800010601
153900010601
154000010330
154100040119      /TITLE Invio dei dati al punto operativo.
154200040119     C     invio         BEGSR
154300040119     C*
154400040119     C* 1° invio FIVAT
154500040119     C                   reset                   dscmz
154600040119     C                   move      vlrpoi        cmzdst
154700040119     C                   eval      cmzfld = 'FIVATWWR'
154800040119     C                   eval      cmzmbd = vlrhdl
154900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155000040119     C***                if        prmfir = *blanks
155100040119     C                   eval      cmzfla = 'FIVAT00F'
155200040119     C                   eval      cmzmba = 'FIVAT00F'
155300040119     C***                else
155400040119     C***                eval      cmzfla = prmfir
155500040119     C***                eval      cmzmba = prmfir
155600040119     C***                endif
155700040119     C                   eval      cmznrr = *zeros
155800040119     C                   move      §ctrokvt      cmznrr
155900040119     C                   eval      cmzlba = vlrfl1
156000040119     C                   call(e)   'TIS711C'
156100040119     C                   parm                    dscmz
156200040119     C                   parm      *blanks       esito
156300040119     C                   if        %error
156400040119     C                             or cmzerr = '1'
156500040119     C                             or esito  = '1'
156600040119     C                   eval      wrkesito = '3'
156700040119     C                   else
156800040119     C*
156900040119     C* 2° invio FIVAB
157000040119     C                   reset                   dscmz
157100040119     C                   move      vlrpoi        cmzdst
157200040119     C                   eval      cmzfld = vlrfou
157300040119     C                   eval      cmzmbd = vlrhdl
157400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
157500040119     C***                if        prmfir = *blanks
157600040119     C                   eval      cmzfla = 'FIVAB00F'
157700040119     C                   eval      cmzmba = 'FIVAB00F'
157800040119     C***                else
157900040119     C***                eval      cmzfla = prmfir
158000040119     C***                eval      cmzmba = prmfir
158100040119     C***                endif
158200040119     C                   eval      cmznrr = *zeros
158300040119     C                   move      §ctrokvb      cmznrr
158400040119     C                   eval      cmzlba = vlrfl1
158500040119     C                   call(e)   'TIS711C'
158600040119     C                   parm                    dscmz
158700040119     C                   parm      *blanks       esito
158800040119     C                   if        %error
158900040119     C                             or cmzerr = '1'
159000040119     C                             or esito  = '1'
159100040119     C                   eval      wrkesito = '3'
159200040119     C                   endif
159300040119     C                   endif
159400040119     C*
159500040119     C                   ENDSR
159600040119     C***
159700010601
159800010601
159900010601
160000010601
160100010601      /TITLE Invio dei dati al punto operativo.
160200010601     C     opeini        BEGSR
160300010601     C*
160400010601     C* Inizializzo flag e contatori operativi
160500010601     C                   movel     '0'           flgGiro           1
160600010601     C                   movel     '0'           flgMulti          1
160700010601     C                   movel     '1'           flgStato          1
160800010615     C                   movel     '0'           flgOk             1
160900010601     C                   z-add     *zeros        cntNonEl         10 0
161000010601     C                   z-add     *zeros        depfil            3 0
161100010601     C                   z-add     *zeros        invfil            3 0
161200010601     C*
161300010601     C                   ENDSR
161400010601     C***
161500010601
161600010601
161700050421
161800050421
161900070411
162000070411     C     *pssr         BEGSR
162100070411     C*
162200070411     C                   if        %open(tivin00r)
162300070411     C                   close     tivin00r
162400070411     C                   endif
162500070411     C                   if        %open(fivabwwr)
162600070411     C                   close     fivabwwr
162700070411     C                   endif
162800070411     C                   if        %open(fivatwwr)
162900070411     C                   close     fivatwwr
163000070411     C                   endif
163100070411     C*
163200070411     C* Effettuo la chiamata al CLLE preposto
163300070411     C                   call(e)   'TITVVTC'
163400070411     C                   parm                    parccm
163500070411     C                   parm                    parmbr
163600070411     C                   parm      '2'           paropz
163700070411     C*
163800070411     C                   eval      wrkesito = '2'
163900070411     C*
164000070411     C                   seton                                        LR
164100070411     C*
164200070411     C                   ENDSR     '*CANCL'
164300070411     C***
164400070411
164500050421
164600010601
164700010330
164800010330
164900000613     C     *inzsr        BEGSR
165000990910     C*
165100990910     C     *entry        plist
165200990920     C                   parm                    tivlrds
165300990921     C                   parm      wrkesito      esito
165400000724     C                   parm                    prmlit
165500000710     C                   parm                    prmfir
165600010330     C*
165700010330     C* CALCOLA LA DATA CORRENTE
165800010330     C                   time                    wn14             14 0
165900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
166000010330     C                   z-add     wn8           g08dat
166100010330     C                   z-add     *zeros        g08inv
166200010330     C                   movel     '0'           g08err
166300010330     C                   call      'XSRDA8'
166400010330     C                   parm                    wlbda8
166500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
166600000613     C*
166700000613     C                   ENDSR
166800000613     C***
166900990908
167000060626     Otitvt4p   E            riepilogo         2
167100990915     O                                              'Upload via Internet'
167200990915     O                                           +1 'Traduzione TIVIN00R -'
167300021025     O                                           +1 'FIVABWWR'
167400990915     O                                           +1 'Testate bolle'
167500990915     O          E            riepilogo   2
167600990915     O                       wrkdata
167700990915     O                       wrkora              +1
167800990915     O                       procname            +1
167900990915     O          E            riepilogo   2
168000990915     O                                              'Cliente..................:'
168100010605     O                       VABCCM        z     +1
168200990915     O          E            riepilogo   2
168300990920     O                                              'Riferimento Strategi.....:'
168400990920     O                       vlrhdl              +1
168500990915     O          E            riepilogo   2
168600990915     O                                              'Giusti...................:'
168700040119     O                       §CTROKVB      2   +  1
168800990915     O          E            riepilogo   2
168900990915     O                                              'Sbagliati e corretti.....:'
169000971022     O                       §CTRMO        2   +  1
169100990915     O          E            riepilogo   2
169200990915     O                                              'Sbagliati e scartati.....:'
169300971022     O                       §CTRNO        2   +  1
169400000613
169500060626     Otitvt4ps  E            testdett          2
169600000613     O                                              'Upload via Internet'
169700000613     O                                           +1 'Traduzione TIVIN00R -'
169800021025     O                                           +1 'FIVABWWR'
169900000613     O                                           +1 'Testate bolle'
170000000616     O          E            testdett    3
170100000613     O                                           +2 'N° rec'
170200000613     O                                           +3 'Anteprima contenuto'
170300000616     O          E            rigadett    2
170400000613     O                       rrnum               +2
170500000621     O                       recko               +3
170600000616     O          E            findett     2
170700000613     O                       wrkdata
170800000613     O                       wrkora              +1
170900000613     O                       procname            +1
171000000616     O          E            findett     2
171100000613     O                                              'Cliente..................:'
171200010605     O                       VABCCM        z     +1
171300000616     O          E            findett     2
171400000613     O                                              'Riferimento Strategi.....:'
171500000613     O                       vlrhdl              +1
171600000616     O          E            findett     2
171700000613     O                                              'Giusti...................:'
171800040119     O                       §CTROKVB      2   +  1
171900000616     O          E            findett     2
172000000613     O                                              'Sbagliati e corretti.....:'
172100000613     O                       §CTRMO        2   +  1
172200000616     O          E            findett     2
172300000613     O                                              'Sbagliati e scartati.....:'
172400000613     O                       §CTRNO        2   +  1
172500000613** CMD - COMANDI CL
172600060626OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
172700060626OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
172800060626DLTOVR FILE(TITVT4P TITVT4PS) LVL(*)
172900010606
173000000613
173100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
173200050202
173300050202
173400050202
173500050202
